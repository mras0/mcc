#include "source.h"
#include "util.h"
#include <fstream>
#include <streambuf>

#include <io.h> // _findfirst

namespace mcc {

std::string_view base_name(const std::string_view base_dir, const std::string& fn) {
    if (fn.compare(0, base_dir.size(), base_dir) == 0) {
        return std::string_view{fn.c_str() + base_dir.size()};
    }
    return fn;
}

std::ostream& operator<<(std::ostream& os, const source_position& pos) {
    int line = 1;
    size_t last_line = 0;
    const auto& t = pos.source().text();
    for (size_t i = 0; i < pos.index(); ++i) {
        if (t[i] == '\n') {
            ++line;
            last_line = i;
        }
    }
    return os << pos.source().name() << ":" << line << ":" << pos.index()-last_line << "-" << pos.index()+pos.length()-last_line;
}

std::string read_file(const std::string& p) {
    std::ifstream in{p};
    if (!in) NOT_IMPLEMENTED("Could not open " << p);
    return std::string{std::istreambuf_iterator<char>{in}, std::istreambuf_iterator<char>{}};
}

std::string normalize_filename(const std::string_view f) {
    std::string cpy{f};
#ifdef _WIN32
    for (auto& ch: cpy) {
        if (ch == '\\') {
            ch = '/';
        } else if (ch >= 'A' && ch <= 'Z') {
            ch |= 0x20;
        }
    }
#endif
    return cpy;
}

void source_manager::add_include_directory(const std::string& dir) {
    auto d = normalize_filename(dir);
    if (d.empty() || d.back() != '/') {
        d.push_back('/');
    }
    include_directories_.push_back(d);
}


// TODO: Load relative to file if using "..."
// TODO: Search include path
const source_file& source_manager::include(const std::string& included_from, const std::string_view filename) {
    assert(filename.size() >= 2 && (filename[0] == '"' || filename[0] == '<'));
    const bool is_local = filename[0] == '"';
    auto fn = std::string{filename.substr(1, filename.size()-2)};

    if (is_local) {
        assert(normalize_filename(included_from) == included_from);
        const auto idx = included_from.find_last_of('/');
        const auto p = idx != std::string::npos ? included_from.substr(0, idx+1) : base_dir_;
        try { 
            return load(p + fn);
        } catch (...) {
            /* ignore error */
        }
    }

    for (const auto& h: standard_headers_) {
        if (h->name() == fn) {
            return *h;
        }
    }

    for (const auto& inc_dir : include_directories_) {
        try {
            return load(inc_dir + fn);
        } catch (...) {
            /* ignore error */
        }
    }
    NOT_IMPLEMENTED("Could not find include " << filename);
}

const source_file& source_manager::load(const std::string_view filename) {
    auto fn = normalize_filename(filename);
    for (const auto& f: files_) {
        if (f->name() == fn) {
            return *f;
        }
    }
    auto content = read_file(fn);
    if (files_.empty()) {
        const auto idx = fn.find_last_of('/');
        if (idx != std::string::npos) {
            base_dir_ = fn.substr(0, idx+1);
        }
    }
    files_.push_back(std::make_unique<source_file>(std::move(fn), std::move(content)));
    return *files_.back();
}

class find_handle {
public:
    explicit find_handle(intptr_t handle) : handle_(handle) {}
    ~find_handle() {
        _findclose(handle_);
    }
    intptr_t get() const { return handle_; }

private:
    intptr_t handle_;
};

std::vector<std::string> process_wild_cards(const std::string& name) {
    auto n = normalize_filename(name);
    if (n.find_first_of("?*") == std::string::npos) {
        return {n};
    }
    auto sep_idx = n.find_last_of('/');
    std::string p{};
    if (sep_idx != std::string::npos) {
        p = n.substr(0, sep_idx+1);
    }
    _finddata_t fi;    
    find_handle handle{_findfirst(n.c_str(), &fi)};
    if (handle.get() == -1) {
        return {};
    }
    std::vector<std::string> files;
    do {
        if (!(fi.attrib & _A_SUBDIR)) {
            files.push_back(p + fi.name);
        }
    } while (_findnext(handle.get(), &fi) == 0);
    return files;
}

}
