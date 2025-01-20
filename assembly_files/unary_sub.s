alphabet[.1-]

_start:
    [1]  <- . then RIGHT and check_right
    [-]  <- self then RIGHT and HALT

get_to_sign:
    [1] <- self then RIGHT and get_to_sign
    [-] <- self then RIGHT and check_right
    [.] <- self then LEFT and restore_initial_state

check_right:
    [-1] <- self then RIGHT and scanright
    [.] <- self then LEFT and restore_initial_state

restore_initial_state:
    [1] <- self then LEFT and restore_initial_state
    [-] <- . then LEFT and restore_initial_state
    [.] <- 1 then RIGHT and HALT

scanright:
    [1-] <- self then RIGHT and scanright
    [.] <- self then LEFT and del_right

del_right:
    [1] <- . then LEFT and check_sign
    [-] <- . then RIGHT and HALT

check_sign:
    [1] <- self then LEFT and scanleft
    [-] <- . then RIGHT and HALT

scanleft:
    [-1] <- self then LEFT and scanleft
    [.] <- self then RIGHT and _start
