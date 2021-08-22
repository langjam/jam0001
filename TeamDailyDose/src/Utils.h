#pragma once

#define CONSTRUCT(klass)                                         \
public:                                                          \
    template<typename Klass = klass, class... Args>              \
    static inline NonnullRefPtr<klass> construct(Args&&... args) \
    {                                                            \
        return adopt_ref(*new Klass(forward<Args>(args)...));    \
    }
