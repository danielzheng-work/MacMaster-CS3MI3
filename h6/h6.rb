# assign an integer for matching corresponding type constructor in Expr
def construct_const(x)
    Expr.new(x,nil,1)
end 

def construct_neg(e)
    Expr.new(e,nil,2)
end 

def construct_abs(e)
    Expr.new(e,nil,3)
end

def construct_plus(l, r)
    Expr.new(l,r,4)
end

def construct_times(l, r)
    Expr.new(l,r,5)
end

def construct_minus(l, r)
    Expr.new(l,r,6)
end

def construct_exp(l, r)
    Expr.new(l,r,7)
end

# Expr class implementation
class Expr
    attr_reader :val
    attr_reader :expr1
    attr_reader :expr2
    attr_reader :type

    def initialize(x, y=nil, i)
        case i
        when 1
            @val = x
        when 2
            @expr1 = x
        when 3
            @expr1 = x
        when 4
            @expr1 = x
            @expr2 = y
        when 5
            @expr1 = x
            @expr2 = y
        when 6
            @expr1 = x
            @expr2 = y
        when 7
            @expr1 = x
            @expr2 = y
        else
            throw "Not vaild argument inputs"
        end
        @type = i
    end

        
# Using type choose the appropriate interpretor
    def interpret
        case @type
        when 1
            return @val
        when 2
            return @expr1.interpret*(-1)
        when 3
            return (@expr1.interpret).abs
        when 4
            return @expr1.interpret + @expr2.interpret
        when 5
            return @expr1.interpret * @expr2.interpret
        when 6
            return @expr1.interpret - @expr2.interpret
        when 7
            return @expr1.interpret.pow(@expr2.interpret)
        else
            throw "Not vaild argument inputs"
        end
    end

end

