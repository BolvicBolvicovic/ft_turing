# This is a test 
alphabet[.1+]
# And that is a new comment
_start:
    [1+] <- 1 then RIGHT and _start
    [.]  <- . then LEFT and delone

delone:
    [1+.] <- . then LEFT and HALT
