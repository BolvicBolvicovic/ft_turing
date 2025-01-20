# This is a test 
alphabet[.1+]
# And that is a new comment
_start_mem:
    [1+] <- 1 then RIGHT and _start_mem
    [.]  <- . then LEFT and delone

delone:
    [1+.] <- . then LEFT and HALT
