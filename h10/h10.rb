Pair = Struct.new(:fst,:snd)


def summingPairs(xs, sum)
  @the_pairs=[]  
  len = xs.length
  reader, writer = IO.pipe
  half = len/2
    threadA= Thread.fork do
        for i in 0..(len-1)
            for j in (i+1)..half
                if xs[i] + xs[j] <= sum
                    @the_pairs.push(Pair.new(xs[i],xs[j]))
                end 
            end
        end
        @the_pairs
    end
    threadB=Thread.fork do
        for i in 0..(len-1)
            for j in (half+1)..(len-1)
                if xs[i] + xs[j] <= sum
                    @the_pairs.push(Pair.new(xs[i],xs[j]))
                end 
            end
        end
        @the_pairs
    end
    threadA.join
    threadB.join
  return @the_pairs
end

