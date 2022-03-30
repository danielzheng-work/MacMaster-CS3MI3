# define method to check if integer is divisible by 3 and 5,
# and change it into corresponding String
def divisible (elem)
    if (elem % 3 == 0 and elem % 5 == 0) then 
        elem = "fizzbuzz"
    elsif elem % 3 == 0 then 
        elem = "fizz" 
    elsif elem % 5 == 0 
        then elem = "buzz"
    else elem = elem.to_s
    end
    return elem
end


# Part 1 : Fizzbuzzing using loop
def fizzbuzzLooper(list)
    list_str = Array.new(list.length)
    for x in 0..(list.length-1) do
        list_str[x] = divisible(list[x])
    end
    return list_str
end

# Part 2 : Fizzbuzzing by iterators
def fizzbuzzIterator(list)
    return list.collect { |x| divisible(x)}
end

# Part 3 : Generalising fizzbuzzing

def zuzzer(list, rules)

    # First, using first rule to create sample string, only change satisfied item into string
    list_str = list.collect do |x|
        x = rules[0][0].call(x)? rules[0][1].call(x) : x
    end

    # iterate using each rule
    for r in rules do
        for i in 0..(list.length-1) do
            # apple rule to each element in list
            if r[0].call(list[i]) then
                # Check if the element is Stirng, then to append the string if it is also satisfied following rules
                (list_str[i].instance_of?(String) and list_str[i] != r[1].call(list[i]) )? (list_str[i] += r[1].call(list[i])) : list_str[i] = r[1].call(list[i]) 
            end
        end
    end    
    # convert all integer in list to string
    return list_str.collect {|x| x.to_s}
end 
