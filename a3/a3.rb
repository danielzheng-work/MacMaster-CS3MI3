module GCL
    class GCExpr 
    end

    class GCConst < GCExpr
        attr_reader :value

        def initialize(int)
            unless int.is_a?(Integer)
                throw "Constructing with non-integer argument"
            end
            @value = int
        end

        def eval_small(result, memory)
            result<<@value
            return result, memory
        end

        def checkScope(list)
            return true
        end

        def eval_big(hash, state)
            return @value, state
        end
    end

    class GCVar < GCExpr
        attr_reader :var
        def initialize(char)
            unless char.is_a?(Symbol)
                throw "Constructing with non-symbol argument"
            end
            @var = char
        end

        def eval_small(result, memory)
            result<< memory.call(@var)
            return result, memory
        end

        def checkScope(list)
            return list.include?(@var)
        end

        def eval_big(hash, state)
            return state.call(@var), state
        end
    end

    class GCOp < GCExpr
        attr_reader :exp1
        attr_reader :exp2
        attr_reader :ops

        def initialize(e1, e2, op)
            unless e1.is_a?(GCExpr) && e2.is_a?(GCExpr) && [:plus, :times, :minus, :div].include?(op)
                throw "Constructing with wrong arguments"
            end
            @exp1 = e1; @exp2= e2; @ops = op
        end
        
        def eval_small(result, memory)
            result, memory = @exp1.eval_small(result, memory)
            result, memory = @exp2.eval_small(result, memory)
            unless result.length == 2
                nil
            end
            case @ops
            when :plus
                result<< (result.pop + result.pop)
                return result, memory
            when :times
                result<< (result.pop * result.pop)
                return result, memory
            when :minus
                result<< (-1*result.pop + result.pop)
                return result, memory
            when :div
                result<< (1.0/result.pop * result.pop).to_i
                return result, memory
            else
                return result, memory
            end
        end

        def checkScope(list)
            return @exp1.checkScope(list) && @exp2.checkScope(list)
        end

        def eval_big(hash, state)
            value1, state = @exp1.eval_big(hash, state)
            value2, state = @exp2.eval_big(hash, state)
            case @ops
            when :plus
                return (value1+value2), state
            when :times
                return value1*value2, state
            when :minus
                return value1-value2, state
            when :div
                return value1/value2, state
            else
                throw "operartor is not avaiable"
            end
        end

    end
    
    class GTTest 
    end

    class GCComp < GTTest
        attr_reader :exp1
        attr_reader :exp2
        attr_reader :ops

        def initialize(e1, e2, op)
            unless e1.is_a?(GCExpr) && e2.is_a?(GCExpr) && [:eq, :less, :greater].include?(op)
                throw "Constructing with wrong arguments"
            end
            @exp1 = e1; @exp2= e2; @ops = op
        end

        def eval_small(result, memory)
            result, memory = @exp1.eval_small(result, memory)
            result, memory = @exp2.eval_small(result, memory)
            unless result.length == 2
                nil
            end
            case @ops
            when :eq
                return result.pop == result.pop
            when :less
                return result.pop > result.pop
            when :greater
                return result.pop < result.pop
            else
                return false
            end
        end

        def checkScope(list)
            return @exp1.checkScope(list) && @exp2.checkScope(list)
        end

        def eval_big(hash, state)
            value1, state = @exp1.eval_big(hash, state)
            value2, state = @exp2.eval_big(hash, state)
            case @ops
            when :eq
                return value1==value2
            when :less
                return value1 < value2
            when :greater
                return value1 > value2
            else
                return false
            end
        end
        
    end

    class GCAnd < GTTest
        attr_reader :pred1
        attr_reader :pred2

        def initialize(t1, t2)
            unless t1.is_a?(GTTest) && t2.is_a?(GTTest)
                throw "Constructing with wrong arguments"
            end
            @pred1=t1; @pred2=t2

        end

        def eval_small(result, memory)
            return @pred1.eval_small(result, memory) && @pred2.eval_small(result, memory)
        end

        def checkScope(list)
            return @pred1.checkScope(list) && @pred2.checkScope(list)
        end

        def eval_big(hash, state)
            return @pred1.eval_big(hash, state) && @pred2.eval_big(hash, state)
        end
    end
    
    class GCOr < GTTest
        attr_reader :pred1
        attr_reader :pred2

        def initialize(t1, t2)
            unless t1.is_a?(GTTest) && t2.is_a?(GTTest)
                throw "Constructing with wrong arguments"
            end
            @pred1=t1; @pred2=t2

        end

        def eval_small(result, memory)
            return @pred1.eval_small(result, memory) || @pred2.eval_small(result, memory)
        end

        def checkScope(list)
            return @pred1.checkScope(list) && @pred2.checkScope(list)
        end

        def eval_big(hash, state)
            return @pred1.eval_big(hash, state) || @pred2.eval_big(hash, state)
        end
    end 

    class GCTrue < GTTest
        
        def eval_small(result, memory) 
            return true 
        end

        def checkScope(list)
            return true
        end

        def eval_big(hash, state)
            return true
        end
    end
    class GCFalse < GTTest

        def eval_small(result, memory)
            return false
        end

        def checkScope(list)
            return true
        end

        def eval_big(hash, state)
            return false
        end
    end


    class GCStmt 
    end
    
    class GCSkip < GCStmt 

        def eval_small (result, memory)
            return result, memory
        end

        def checkScope(list)
            return true
        end

        def eval_big(hash, state)
            return hash, state
        end
    end
    
    class GCAssign < GCStmt
        attr_reader :var
        attr_reader :exp

        def initialize(v, e)
            unless v.is_a?(Symbol) && e.is_a?(GCExpr)
                throw "Constructing with wrong arguments"
            end
            @var = v ; @exp=e
        end

        def eval_small(result, memory)
            result, memory = @exp.eval_small(result, memory)
            memory = updateState(memory,@var, result.pop)
            result<< memory.call(@var)
            return result, memory
        end

        def checkScope(list)
            return list.include?(@var) && @exp.checkScope(list)
        end

        def eval_big(hash, state)
            value, state = @exp.eval_big(hash, state)
            state = update(state, @var, value)
            hash[@var] = value
            return hash, state
        end



    end

    class GCCompose < GCStmt
        attr_reader :stat1
        attr_reader :stat2

        def initialize(s1, s2)
            unless s1.is_a?(GCStmt) && s2.is_a?(GCStmt)
                throw "Constructing with wrong arguments"
            end
            @stat1 = s1 ; @stat2=s2
        end

        def eval_small(result, memory)
            result, memory = @stat1.eval_small(result, memory)
            result, memory = @stat2.eval_small(result, memory)
            return result, memory 
        end

        def checkScope(list)
            return @stat1.checkScope(list) && @stat2.checkScope(list)
        end

        def eval_big(hash, state)
            hash, state = @stat1.eval_big(hash, state)
            hash, state = @stat2.eval_big(hash, state)
            return hash, state
        end
    end

    class GCIf < GCStmt
        attr_reader :list

        def initialize(l)
            unless l.is_a?(Array)
                throw "Constructing with wrong arguments"
            end
            @list = l
        end
        
        def eval_small(result, memory)
            pick = @list.sample
            if pick[0].eval_small(result, memory)
                result, memory = pick[1].eval_small(result, memory)
                return result, memory
            else
                return result, memory
            end
        end

        def checkScope(list)
            @list.each do |pair|
                unless pair[0].checkScope(list) && pair[1].checkScope(list)
                    return false
                end
            end
            return true
        end

        def eval_big(hash, state)
            pick = @list.sample
            if pick[0].eval_big(hash, state)
                hash,state = pick[1].eval_big(hash, state)
                return hash, state
            else
                return hash,state
            end
        end



    end

    class GCDo < GCStmt
        attr_reader :list

        def initialize(l)
            unless l.is_a?(Array)
                throw "Constructing with wrong arguments"
            end
            @list = l
        end

        def eval_small(result, memory)
            loop do
                pick = @list.sample
                result, memory = pick[1].eval_small(result, memory)
                if pick[0].eval_small(result, memory) == false
                    return result, memory
                    break
                end
            end
        end

        def checkScope(list)
            @list.each do |pair|
                unless pair[0].checkScope(list) && pair[1].checkScope(list)
                    return false
                end
            end
            return true
        end

        def eval_big(hash, state)
            loop do
                pick = @list.sample
                hash,state= pick[1].eval_big(hash, state)
                if pick[0].eval_big(hash, state) == false
                    return hash, state
                    break
                end
            end
        end
    end
    def stackEval(command, result, memory) 
        command.each do |exp|
            result, memory = exp.eval_small(result, memory)
        end
        result.clear
        return memory
    end
    
    def emptyState
        return lambda{|var| 0}
    end

    def updateState(lam, var, int)
        return lambda{|v|
            if v == var
                int 
            else
                lam.call(v)
            end
        }
    end


    
end

module GCLe
    include GCL
    class GCProgram
        attr_reader :vars
        attr_reader :stat

        def initialize(vs, st)
            unless vs.is_a?(Array) && st.is_a?(GCStmt)
                throw "Constructing with wrong arguments"
            end
            @vars=vs; @stat=st
        end
    end
    class GCLocal < GCStmt
        attr_reader :var
        attr_reader :stat

        def initialize(v,s)
            unless v.is_a?(Symbol) && s.is_a?(GCStmt)
                throw "Constructing with wrong arguments"
            end
            @var=v; @stat=s
        end

        def checkScope(list)
            list<<@var
            return @stat.checkScope(list)
        end

        def eval_big(hash, state)
            return @stat.eval_big(hash, state)
        end
    end

    def wellScoped(program)
        unless program.is_a?(GCProgram)
            throw "wrong argument"
        end
        var_list=program.vars
        return program.stat.checkScope(var_list)
    end

    def eval(program)
        unless program.is_a?(GCProgram)
            throw "wrong argument"
        end
        if wellScoped(program)
            values=Hash.new
            hash, state= program.stat.eval_big(values, empty)
            return hash
        else
            throw "some variables are not well declared"
        end
    end

    def empty
        return lambda{|var| 0}
    end

    def update(lam, var, int)
        return lambda{|v|
            if v == var
                int 
            else
                lam.call(v)
            end
        }
    end
end