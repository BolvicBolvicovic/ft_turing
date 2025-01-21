# This is a test 
alphabet[.10+]
# And that is a new comment
_start_mem:
    [1] <-  . inc eax then RIGHT and _start_mem
    [+] <-  . mov ebx int(6969) then RIGHT and _start_mem
    [0] <-  . mov ebx int(4200) then RIGHT and test
    [.]  <- . eq ebx int(6969) :true then LEFT and HALT else LEFT and ERROR

test:
    [.10]  <- self eq ebx int(6969) :true then LEFT and _start_mem else LEFT and _start_mem
