#pragma once

#include <AK/String.h>
#include <AK/Variant.h>

class Block;

using Value = Variant<String, NonnullRefPtr<Block>>;
