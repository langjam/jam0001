#pragma once

#include <stdint.h>
#include <csetjmp>
#include <cstdint>
#include <deque>
#include <iostream>
#include <functional>
#include <mutex>
#include <ostream>
#include <pthread.h>
#include <cstdlib>
#include <sched.h>
#include <assert.h>
#include <string>
#include <sstream>
#include <tuple>
#include <unordered_map>
#include <variant>
#include <memory>
#include <vector>

#ifndef HAS_ATOMS
namespace atom {
enum atom {};
}
#endif

namespace slapi {

struct WorkID {
    uint64_t id;
};
struct Work {
    uint64_t id;
    atom::atom origin;
    void* inner;
};
using ref = std::variant<std::int64_t, /* nil */ nullptr_t, std::string, bool, atom::atom, WorkID, Work>;

inline ref nil = ref(nullptr);

struct Thread {
    atom::atom name;
    std::unordered_map</* wdeque name */ atom::atom, /* progress */ std::uint64_t> progressmap;
};

static std::unordered_map<atom::atom, std::vector<std::tuple<ref, std::deque<ref>>>>
    messages;
static std::int64_t threads_online = 1;
static std::mutex thmutex;
static thread_local Thread current;

static void* init_thread(void* tgd) {
    std::function<ref()>* tgdtrue = reinterpret_cast<std::function<ref()>*>(tgd);
    (*tgdtrue)();
    thmutex.lock();
    threads_online--;
    if (threads_online == 0) {
        exit(0);
    }
    thmutex.unlock();
    pthread_exit(NULL);
}

void main_thread_go_bye() {
    thmutex.lock();
    threads_online--;
    thmutex.unlock();
    if (threads_online == 0) {
        exit(0);
    }
    pthread_exit(nullptr);
}

static void new_thread_from_fn(std::function<ref()> tgd) {
    thmutex.lock();
    threads_online++;
    thmutex.unlock();
    pthread_t thr;
    pthread_create(
        (pthread_t*)malloc(sizeof(pthread_t)), nullptr, init_thread, (void*)new std::function<ref()>(tgd));
}

static void set_thread_name(atom::atom name) {
    current.name = name;
    messages[current.name] = std::vector<std::tuple<ref, std::deque<ref>>>{};
}

static ref from_number(std::int64_t value) {
    return ref(value);
}
static ref from_bool(bool value) {
    return ref(value);
}
static ref from_str(std::string value) {
    return ref(value);
}
static bool to_bool(ref r) {
    return std::holds_alternative<bool>(r) ? std::get<bool>(r) : false;
}
static ref from_symbol(atom::atom value) {
    return ref(value);
}
static std::mutex workm;
static ref pubwork(ref val) {
    workm.lock();
    messages[current.name].push_back(std::tuple<ref, std::deque<ref>>{val, std::deque<ref>{}});
    ref r = ref(WorkID{messages[current.name].size() - 1});
    workm.unlock();
    return r;
}
static ref rfc(ref work) {
    WorkID wid = std::get<WorkID>(work);
    while (true) {
        workm.lock();
        auto& q = std::get<1>(messages[current.name][wid.id]);
        if (!q.empty()) {
            auto val = q.front();
            q.pop_front();
            workm.unlock();
            return val;
        }
        workm.unlock();
        sched_yield();
    }
}
static ref eq(ref l, ref r) {
    if (l.index() != r.index()) return ref(false);
    if (std::holds_alternative<std::int64_t>(l))
        return std::get<std::int64_t>(l) == std::get<std::int64_t>(r);
    if (std::holds_alternative<nullptr_t>(l)) return true;
    if (std::holds_alternative<std::string>(l)) return std::get<std::string>(l) == std::get<std::string>(r);
    if (std::holds_alternative<bool>(l)) return std::get<bool>(l) == std::get<bool>(r);
    if (std::holds_alternative<atom::atom>(l)) return std::get<atom::atom>(l) == std::get<atom::atom>(r);
    if (std::holds_alternative<WorkID>(l))
        return std::get<WorkID>(l).id == std::get<WorkID>(r).id;
    assert(!"unknown case (UB?)");
    abort();
}
static std::string format(ref r) {
    if (std::holds_alternative<std::int64_t>(r)) {
        std::string s;
        std::stringstream out;
        out << std::get<std::int64_t>(r);
        return out.str();
    }
    if (std::holds_alternative<nullptr_t>(r)) return "nil";
    if (std::holds_alternative<std::string>(r)) return std::get<std::string>(r);
    if (std::holds_alternative<bool>(r)) return std::get<bool>(r) ? "true" : "false";
    if (std::holds_alternative<atom::atom>(r)) return "<atom>";
    if (std::holds_alternative<WorkID>(r)) return "<work>";
    if (std::holds_alternative<Work>(r)) return std::string{"work{"} + format(*(ref*)std::get<Work>(r).inner) + std::string{"}"};
    return "<unknown>";
}
static void log(ref r) {
    std::cout << "[+] [" << atom2str(current.name) << "] " << format(r) << std::endl;
}
static ref add(ref l, ref r) {
    return ref(std::get<int64_t>(l) + std::get<int64_t>(r));
}
static ref seework(atom::atom a) {
    if (!current.progressmap.contains(a)) {
        current.progressmap[a] = 0;
    }
    while (true) {
        workm.lock();
        if (!messages.contains(a)) {
            workm.unlock();
            sched_yield();
            continue;
        }
        if (messages[a].size() == current.progressmap[a]) {
            workm.unlock();
            sched_yield();
            continue;
        }
        auto v = std::get<0>(messages[a].at(current.progressmap[a]));
        workm.unlock();
        
        return ref(Work{
            .id = current.progressmap[a]++,
            .origin = a,
            .inner = (void*)(new ref(v))
        });
    }
}
static ref comment(ref v, ref pld) {
    auto wp = std::get<Work>(v);
    workm.lock();
    std::get<1>(messages[wp.origin][wp.id]).push_back(pld);
    workm.unlock();
    return ref(nullptr);
}

} // namespace slapi

using slref = slapi::ref;