// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Allocator.h"
#include <iostream>

static void* luauDefaultMallocPtr(void* ctx, size_t size)
{
    return malloc(size);
}

// wrap free
static void luauDefaultFreePtr(void* ctx, void* ptr)
{
    free(ptr);
}

static Luau::AllocatorVTable CVTableAllocator = {luauDefaultMallocPtr, luauDefaultFreePtr};

namespace Luau
{

Allocator::Allocator()
    : root(nullptr)
    , offset(0)
    , vtable(CVTableAllocator)
    , ctx(nullptr)
{
    allocate(0);
}

Allocator::Allocator(Allocator&& rhs)
    : root(rhs.root)
    , offset(rhs.offset)
    , vtable(rhs.vtable)
    , ctx(rhs.ctx)
{
    rhs.root = nullptr;
    rhs.offset = 0;
}

Allocator::Allocator(void* ctx, AllocatorVTable vtable)
    : root(nullptr)
    , offset(0)
    , vtable(vtable)
    , ctx(ctx)
{
    allocate(0);
}

Allocator::~Allocator()
{
    Page* page = root;

    while (page != nullptr)
    {
        Page* next = page->next;

        vtable.freePtr(ctx, page);

        page = next;
    }
}

void* Allocator::allocate(size_t size)
{
    constexpr size_t align = alignof(void*) > alignof(double) ? alignof(void*) : alignof(double);

    if (root != nullptr)
    {
        uintptr_t data = reinterpret_cast<uintptr_t>(root->data);
        uintptr_t result = (data + offset + align - 1) & ~(align - 1);
        if (result + size <= data + sizeof(root->data))
        {
            offset = result - data + size;
            return reinterpret_cast<void*>(result);
        }
    }

    // allocate new page
    size_t pageSize = size > sizeof(root->data) ? size : sizeof(root->data);
    void* pageData = vtable.mallocPtr(ctx, offsetof(Page, data) + pageSize);

    Page* page = static_cast<Page*>(pageData);

    page->next = root;

    root = page;
    offset = size;

    return page->data;
}

} // namespace Luau
