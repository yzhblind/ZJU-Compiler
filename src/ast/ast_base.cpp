#include "ast_base.hpp"

void ASTNode::set_location(uint32_t line_no, uint32_t column_no)
{
    this->line_no = line_no, this->column_no = column_no;
}

uint32_t ASTNode::get_line_no() const
{
    return line_no;
}

uint32_t ASTNode::get_col_no() const
{
    return column_no;
}

ASTNode::~ASTNode()
{
}
