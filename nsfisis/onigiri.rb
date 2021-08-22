module Token
  Name = Struct.new(:name)
  Comment = Struct.new(:content)
  Literal = Struct.new(:value)
end

module AST
  Program = Struct.new(:stmts)
  StmtWithComment = Struct.new(:stmt, :comment)
  If = Struct.new(:cond, :then, :else)
  While = Struct.new(:cond, :block)
  Var = Struct.new(:name, :init)
  BinaryExpr = Struct.new(:op, :lhs, :rhs)
  UnaryExpr = Struct.new(:op, :operand)
  Call = Struct.new(:func, :args)
  Literal = Struct.new(:value)
end

Comment = Struct.new(:content, :target) do
  def to_s
    content
  end
end



class Lexer
  def initialize(source)
    @source = source
    @index = 0

    next!
  end

  def next!
    get
    nil
  end

  def get
    ret = @token
    @token = scan_next
    ret
  end

  def peek
    @token
  end

  def eof?
    peek == :EOF
  end

  def all_tokens
    prev_index = @index
    tokens = []
    until eof?
      tokens << get
    end
    @index = prev_index
    tokens
  end

  private

  def scan_next
    if @source.length <= @index
      return :EOF
    end

    case @source[@index]
    when /[ \t\n\r]/
      while @index < @source.length && @source[@index] =~ /[ \t\n\r]/
        @index += 1
      end
      scan_next
    when /[-+\/%*{}(),;]/
      @index += 1
      @source[@index - 1].to_sym
    when '='
      @index += 1
      case @source[@index]
      when '='
        @index += 1
        :'=='
      else
        :'='
      end
    when '<'
      @index += 1
      case @source[@index]
      when '='
        @index += 1
        :'<='
      else
        :'<'
      end
    when '>'
      @index += 1
      case @source[@index]
      when '='
        @index += 1
        :'>='
      else
        :'>'
      end
    when '!'
      @index += 1
      if @source[@index] == '='
        @index += 1
        :'!='
      else
        :'!'
      end
    when /[0-9]/
      num = ''
      while @index < @source.length && @source[@index] =~ /[0-9]/
        num += @source[@index]
        @index += 1
      end
      Token::Literal.new(num.to_i)
    when '"'
      str = ''
      @index += 1 # skip a double quote.
      while @index < @source.length && @source[@index] != '"'
        if @source[@index] == "\\"
          @index += 1
          case @source[@index]
          when 'n'
            str += "\n"
          when 't'
            str += "\t"
          when '"'
            str += '"'
          when "\\"
            str += "\\"
          else
            raise "Unknown escape sequence"
          end
        else
          str += @source[@index]
        end
        @index += 1
      end
      @index += 1 # skip a double quote.
      Token::Literal.new(str)
    when /[a-zA-Z_]/
      name = ''
      while @index < @source.length && @source[@index] =~ /[a-zA-Z_0-9]/
        name += @source[@index]
        @index += 1
      end
      case name
      when 'commentof'
        :commentof
      when 'else'
        :else
      when 'elseif'
        :elseif
      when 'if'
        :if
      when 'var'
        :var
      when 'while'
        :while
      else
        Token::Name.new(name.to_sym)
      end
    when '#'
      @index += 1 # skip a number sign.
      line = ''
      while @index < @source.length && @source[@index] !~ /[\r\n]/
        line += @source[@index]
        @index += 1
      end
      Token::Comment.new(line.strip)
    else
      raise "Invalid character: #{@source[@index]}"
    end
  end
end

class Parser
  def initialize(lexer)
    @lexer = lexer
  end

  def parse
    parse_program
  end

  private

  def parse_program
    stmts = []
    until @lexer.eof?
      if (stmt = parse_stmt_c)
        stmts << stmt
      end
    end
    AST::Program.new(stmts)
  end

  def parse_stmt_c
    if Token::Comment === @lexer.peek
      comment = ''
      while Token::Comment === @lexer.peek
        comment += @lexer.get.content + "\n"
      end
      comment = Comment.new(comment.strip, nil)
    else
      comment = Comment.new('', nil)
    end
    stmt = parse_stmt || return
    if AST::Var === stmt
      comment.target = stmt.name
    end
    AST::StmtWithComment.new(stmt, comment)
  end

  def parse_stmt
    case @lexer.peek
    when :';'
      @lexer.next!
    when :if
      parse_if_stmt
    when :while
      parse_while_stmt
    when Token::Comment
      comment = ''
      while Token::Comment === @lexer.peek
        comment += @lexer.get.content + "\n"
      end
      parse_var_def(comment.strip)
    when :var
      parse_var_def
    else
      stmt = parse_expr
      expect(:';')
      stmt
    end
  end

  def parse_if_stmt
    @lexer.next!
    cond = parse_cmp_expr
    then_block = parse_block
    else_block = case @lexer.peek
      when :elseif
        parse_if_stmt
      when :else
        @lexer.next!
        parse_block
      else
        nil
      end
    AST::If.new(cond, then_block, else_block)
  end

  def parse_while_stmt
    @lexer.next!
    cond = parse_cmp_expr
    block = parse_block
    AST::While.new(cond, block)
  end

  def parse_var_def
    @lexer.next!
    name = parse_name
    init = case @lexer.get
      when :';'
        AST::Literal.new(0)
      when :'='
        parse_cmp_expr
      else
        raise "expect ';' or '='"
      end
    AST::Var.new(name, init)
  end

  def parse_params
    expect(:'(')
    params = []
    while !@lexer.eof? && @lexer.peek != :')'
      params << parse_name
      if @lexer.peek == :','
        @lexer.next!
      else
        break
      end
    end
    expect(:')')
    params
  end

  def parse_block
    expect(:'{')
    stmts = []
    while !@lexer.eof? && @lexer.peek != :'}'
      if (stmt = parse_stmt_c)
        stmts << stmt
      end
    end
    expect(:'}')
    stmts
  end

  def parse_expr
    parse_assign_expr
  end

  def parse_assign_expr
    lhs = parse_cmp_expr
    if @lexer.peek == :'='
      raise "LHS of assignment must be a variable" unless Symbol === lhs
      @lexer.next!
      rhs = parse_cmp_expr
      AST::BinaryExpr.new(:'=', lhs, rhs)
    else
      lhs
    end
  end

  def parse_cmp_expr
    lhs = parse_add_expr
    case @lexer.peek
    when :'==', :'!=', :'<', :'>', :'<=', :'>='
      op = @lexer.get
      rhs = parse_add_expr
      AST::BinaryExpr.new(op, lhs, rhs)
    else
      lhs
    end
  end

  def parse_add_expr
    lhs = parse_mul_expr
    loop do
      case @lexer.peek
      when :'+', :'-'
        op = @lexer.get
        rhs = parse_mul_expr
        lhs = AST::BinaryExpr.new(op, lhs, rhs)
      else
        return lhs
      end
    end
  end

  def parse_mul_expr
    lhs = parse_prefix_expr
    loop do
      case @lexer.peek
      when :'*', :'/', :'%'
        op = @lexer.get
        rhs = parse_prefix_expr
        lhs = AST::BinaryExpr.new(op, lhs, rhs)
      else
        return lhs
      end
    end
  end

  def parse_prefix_expr
    case @lexer.peek
    when :commentof
      op = @lexer.next!
      operand = parse_name
      AST::UnaryExpr.new(:commentof, operand)
    when :commentof, :'!', :'-', :'+'
      op = @lexer.get
      operand = parse_prefix_expr
      AST::UnaryExpr.new(op, operand)
    else
      parse_postfix_expr
    end
  end

  def parse_postfix_expr
    expr = parse_primary_expr
    loop do
      if @lexer.peek == :'('
        args = parse_args
        expr = AST::Call.new(expr, args)
      else
        return expr
      end
    end
  end

  def parse_args
    expect(:'(')
    args = []
    while !@lexer.eof? && @lexer.peek != :')'
      args << parse_cmp_expr
      if @lexer.peek == :','
        @lexer.next!
      else
        break
      end
    end
    expect(:')')
    args
  end

  def parse_primary_expr
    case (t = @lexer.get)
    when :'('
      expr = parse_expr
      expect(:')')
      expr
    when Token::Name
      t.name
    when Token::Literal
      AST::Literal.new(t.value)
    else
      raise "expect a primary expression, but #{t.inspect}"
    end
  end

  def parse_name
    var = @lexer.get
    raise "expect NAME, but #{var.inspect}" unless Token::Name === var
    var.name
  end

  def expect(t)
    if @lexer.peek != t
      raise "expect '#{t}', but '#{@lexer.peek}'"
    end
    @lexer.next!
  end
end


class Runtime
  def initialize(ast)
    @ast = ast
    @vars = {}
    @comments = {}
  end

  def run
    eval(@ast)
  end

  def comments
    @comments.each do |name, comment|
      next if comment.to_s.empty?

      puts "========================="
      puts name
      puts "-------------------------"
      puts comment.to_s
      puts
    end
  end

  private

  def get_var(name)
    raise "undefined variable #{name}" unless @vars[name]
    @vars[name]
  end

  def set_var(name, value)
    raise "undefined variable #{name}" unless @vars[name]
    if Comment === @vars[name]
      @vars[name] = set_commentof(@vars[name].target, value)
    else
      @vars[name] = value
    end
  end

  def define_var(name, init)
    raise "duplicate variable #{name}" if @vars[name]
    @vars[name] = init
  end

  def get_commentof(name)
    raise "undefined variable #{name}" unless @vars[name]
    @comments[name] || ''
  end

  def set_commentof(name, content)
    @comments[name] = Comment.new(content.to_s, name)
  end

  def append_commentof(name, content)
    @comments[name] = Comment.new(@comments[name].to_s + "\n" + content.to_s, name)
  end

  def eval(ast)
    case ast
    when AST::Program
      eval_program(ast)
    when AST::StmtWithComment
      eval_stmt_c(ast)
    when AST::If
      eval_if(ast)
    when AST::While
      eval_while(ast)
    when AST::Var
      eval_var_def(ast)
    when AST::BinaryExpr
      eval_binary_expr(ast)
    when AST::UnaryExpr
      eval_unary_expr(ast)
    when AST::Call
      eval_call(ast)
    when AST::Literal
      ast.value
    when Symbol
      get_var(ast)
    when Array
      ast.each do |stmt|
        eval(stmt)
      end
    else
      raise "unknown AST"
    end
  end

  def eval_program(ast)
    eval(ast.stmts)
  end

  def eval_stmt_c(ast)
    ret = eval(ast.stmt)
    case ast.stmt
    when AST::Var
      set_commentof(ast.stmt.name, ast.comment)
    when AST::BinaryExpr
      if ast.stmt.op == :'='
        append_commentof(ast.stmt.lhs, ast.comment)
      end
    end
    ret
  end

  def eval_if(ast)
    cond = eval(ast.cond)
    if cond != 0
      eval(ast.then)
    else
      eval(ast.else)
    end
  end

  def eval_while(ast)
    loop do
      cond = eval(ast.cond)
      if cond == 0
        break
      end
      eval(ast.block)
    end
  end

  def eval_var_def(ast)
    define_var(ast.name, eval(ast.init))
  end

  def eval_binary_expr(ast)
    case ast.op
    when :'='
      set_var(ast.lhs, eval(ast.rhs))
    when :'=='
      (eval(ast.lhs) == eval(ast.rhs)) ? 1 : 0
    when :'!='
      (eval(ast.lhs) != eval(ast.rhs)) ? 1 : 0
    when :'<'
      (eval(ast.lhs) < eval(ast.rhs)) ? 1 : 0
    when :'>'
      (eval(ast.lhs) > eval(ast.rhs)) ? 1 : 0
    when :'<='
      (eval(ast.lhs) <= eval(ast.rhs)) ? 1 : 0
    when :'>='
      (eval(ast.lhs) >= eval(ast.rhs)) ? 1 : 0
    when :'+'
      lhs = eval(ast.lhs)
      rhs = eval(ast.rhs)
      case
      when Comment === lhs
        Comment.new(lhs.to_s + rhs.to_s, nil)
      when Comment === rhs
        Comment.new(lhs.to_s + rhs.to_s, nil)
      else
        lhs + rhs
      end
    when :'-'
      eval(ast.lhs) - eval(ast.rhs)
    when :'*'
      eval(ast.lhs) * eval(ast.rhs)
    when :'/'
      eval(ast.lhs) / eval(ast.rhs)
    when :'%'
      eval(ast.lhs) % eval(ast.rhs)
    end
  end

  def eval_unary_expr(ast)
    case ast.op
    when :commentof
      get_commentof(ast.operand)
    when :'!'
      eval(ast.operand) == 0 ? 1 : 0
    when :'+'
      eval(ast.operand)
    when :'-'
      -eval(ast.operand)
    end
  end

  def eval_call(ast)
    args = ast.args.map {|arg| eval(arg)}
    case ast.func
    when :print
      puts args
    else
      raise "Unknown variable"
    end
  end

  def eval_literal(ast)
    ast.value
  end
end


l = Lexer.new(ARGF.read)
p = Parser.new(l)
ast = p.parse
r = Runtime.new(ast)
r.run
r.comments
