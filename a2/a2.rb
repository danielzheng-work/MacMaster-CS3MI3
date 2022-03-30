require_relative "a2_ulterm"
class STType end

    class STNat < STType
    # Comparison and printing methods
    def ==(type); type.is_a?(STNat) end
    def to_s; "nat" end
    end
    
    class STBool < STType
    # Comparison and printing methods
    def ==(type); type.is_a?(STBool) end
    def to_s; "bool" end
    end
    
    # Functions have a domain type and a codomain type.
    class STFun < STType
    attr_reader :dom
    attr_reader :codom
    
    def initialize(dom, codom)
        unless dom.is_a?(STType) && dom.is_a?(STType)
        throw "Constructing a type out of non-types"
        end
        @dom = dom; @codom = codom
    end
    
    # Comparison and printing methods
    def ==(type); type.is_a?(STFun) && type.dom == @dom && type.codom == @codom end 
    def to_s; "(" + dom.to_s + ") -> (" + codom.to_s + ")" end
    end

# Example use: the type "nat -> bool" is written STFun.new(STNat.new,STBool.new)

# Part 1
class STTerm 
    def typecheck
        if self.typeOf(-1, []).nil?
            return false
        else
            return true
        end
    end
end

class STVar < STTerm
    attr_reader :index

    def initialize(i)
        unless i.is_a?(Integer)
            throw "Constructing a lambda term out of non-lambda terms"
        end
        @index = i
    end

    def ==(term); term.is_a?(STVar) && r.index == @index end
    def to_s; @index.to_s end

    def typeOf(currentB, list)
        if @index > currentB
            return nil
        else
            return STNat.new
        end
    end

    def eraseTypes
        return ULVar.new(@index)
    end

end

class STApp < STTerm
    attr_reader :t1
    attr_reader :t2

    def initialize(t1, t2)
        unless t1.is_a?(STTerm) && t2.is_a?(STTerm)
            throw "Constructing a lambda term out of non-lambda terms"
        end
        @t1 = t1; @t2 = t2
    end

    def ==(app); app.is_a?(STApp) && app.t1 == @t1 && app.t2 == @t2 end
    def to_s; "(" + @t1.to_s + ") (" + @t2.to_s + ")" end

    def typeOf(currentB, list)
        if @t1.typeOf(currentB, list).is_a?(STFun)
            if @t2.typeOf(currentB, list) == @t1.typeOf(currentB, list).dom
                return @t1.typeOf(currentB,list).codom
            end
        end
        return nil
    end
    
    def eraseTypes
        return ULApp.new(@t1.eraseTypes, @t2.eraseTypes)
    end


end

class STAbs < STTerm
    attr_reader :type
    attr_reader :term

    def initialize(v,t)
        unless v.is_a?(STType) && t.is_a?(STTerm)
            throw "Constructing a lambda term out of non-lambda terms"
        end
        @type = v; @term = t
    end

    def ==(abs); abs.is_a?(STAbs) && abs.type = @type && abs.term = @term end
    def to_s; "lambda : " + @type.to_s + " . " + @term.to_s end

    def typeOf(currentB, list)
        if @type.is_a?(STFun)
            if @term.is_a?(STAbs)
                if @term.type == @type.dom
                    return STFun.new(@type,@type)
                end
            elsif @term.is_a?(STVar)
                if @term.index <= currentB+1
                    return STFun.new(@type, @type)
                end
            else
                return nil
            end
        else
            if @term.typeOf(currentB+1, nil).nil?
                return nil
            else
                return STFun.new(@type,@term.typeOf(currentB+1, nil))
            end
        end
    end

    def eraseTypes
        return ULAbs.new(@term.eraseTypes) 
    end

end

class STZero < STTerm
    
    def ==(n); n.is_a?(STZero) end
    def to_s; "zero" end

    def typeOf(currentB, list)
        return STNat.new
    end

    def eraseTypes
        return ULAbs.new(ULAbs.new(ULVar.new(0)))
    end

end 

class STSuc < STTerm
    attr_reader :term

    def initialize(t)
        unless t.is_a?(STTerm)
            throw "Constructing a lambda term out of non-lambda terms"
        end
        @term = t
    end

    def ==(suc); suc.is_a?(STSuc) && suc.term == @term end
    def to_s; "suc(" + @term.to_s + ")" end

    def typeOf(currentB, list)
        if @term.typeOf(currentB, list).is_a?(STNat)
            return STNat.new
        else
            return nil
        end
    end

    def eraseTypes
        return ULApp.new(ULAbs.new(ULAbs.new(ULAbs.new(ULApp.new(ULVar.new(1),
            ULApp.new(ULApp.new(ULVar.new(2),ULVar.new(1)),ULVar.new(0)))))), @term.eraseTypes)
    end
end

class STIsZero < STTerm
    attr_reader :term
    
    def initialize(t)
        unless t.is_a?(STTerm)
            throw "Constructing a lambda term out of non-lambda terms"
        end
        @term = t
    end

    def ==(t2); t2.is_a?(STIsZero) && t2.term == @term end
    def to_s; "iszero(" + @term.to_s + ")" end

    def typeOf(currentB, list)
        if @term.typeOf(currentB, list).is_a?(STNat)
            return STBool.new
        else
            return nil
        end
    end

    def eraseTypes
        return ULApp.new(ULAbs.new(ULApp.new(ULApp.new(ULVar.new(0),ULAbs.new(STFalse.new.eraseTypes)),STTrue.new.eraseTypes)), @term.eraseTypes)
    end


end

class STTrue < STTerm

    def ==(t); t.is_a?(STTrue) end
    def to_s; "true" end

    def typeOf(currentB, list)
        return STBool.new
    end

    def eraseTypes
        return ULAbs.new(ULAbs.new(ULVar.new(1)))
    end
end

class STFalse < STTerm

    def ==(t); t.is_a?(STFalse) end
    def to_s; "false" end

    def typeOf(currentB, list)
        return STBool.new
    end

    def eraseTypes
        return ULAbs.new(ULAbs.new(ULVar.new(0)))
    end
end

class STTest < STTerm
    attr_reader :t1
    attr_reader :t2
    attr_reader :t3

    def initialize(t1, t2, t3)
        unless t1.is_a?(STTerm) && t2.is_a?(STTerm) && t3.is_a?(STTerm)
            throw "Constructing a lambda term out of non-lambda terms"
        end 
        @t1 = t1; @t2 = t2; @t3 = t3
    
    end

    def ==(test); test.is_a?(STTest) && test.t1 == @t1 && test.t2 == @t2 && test.t3 == @t3 end
    def to_s;  "(" + @t1.to_s + ") (" + @t2.to_s + ") (" + @t3.to_s + ")" end

    def typeOf(currentB, list)
        if @t1.typeOf(currentB, list).is_a?(STBool)
            if @t2.typeOf(currentB,nil) == @t3.typeOf(currentB, list)
                return @t2.typeOf(currentB, list)
            end
        else
            return nil
        end
    end

    def eraseTypes
        return ULApp.new(ULApp.new(ULAbs.new(ULAbs.new(ULAbs.new(ULApp.new(
            ULApp.new(ULVar.new(2), ULVar.new(1)),ULVar.new(0))))), @t2.eraseTypes), @t3.eraseTypes)
    end

end
