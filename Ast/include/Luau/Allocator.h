// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Ast.h"
#include "Luau/Location.h"
#include "Luau/DenseHash.h"
#include "Luau/Common.h"

#include <vector>

namespace Luau
{

constexpr size_t DEFAULT_ALLOCATOR_PAGE_SIZE = 8192;
constexpr size_t DEFAULT_ALLOCATOR_ALIGNMENT = 8;

extern "C" struct AllocatorVTable
{
    void* (*mallocPtr)(void* ctx, size_t size);
    void (*freePtr)(void* ctx, void* ptr);
};

class Allocator
{
public:
    Allocator();
    Allocator(Allocator&&);
    Allocator(void* ctx, AllocatorVTable vtable);

    Allocator& operator=(Allocator&&) = delete;

    ~Allocator();

    void* allocate(size_t size);

    template<typename T, typename... Args>
    T* alloc(Args&&... args)
    {
        static_assert(std::is_trivially_destructible<T>::value, "Objects allocated with this allocator will never have their destructors run!");

        T* t = static_cast<T*>(allocate(sizeof(T)));
        new (t) T(std::forward<Args>(args)...);
        return t;
    }

private:
    struct Page
    {
        Page* next;

        alignas(DEFAULT_ALLOCATOR_ALIGNMENT) char data[DEFAULT_ALLOCATOR_PAGE_SIZE];
    };

    Page* root;
    size_t offset;

    AllocatorVTable vtable;
    void* ctx;
};

} // namespace Luau
