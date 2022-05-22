#pragma once

#include <vector>
#include <cstdint>
#include <cstdlib>

class ASTNode
{
public:
    virtual ~ASTNode() = 0;
    void set_location(uint32_t line_no, uint32_t column_no);
    uint32_t get_line_no() const;
    uint32_t get_col_no() const;

private:
    uint32_t line_no, column_no;
};