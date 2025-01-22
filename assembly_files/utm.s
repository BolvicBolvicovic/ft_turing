# RIGHT = 0
# LEFT  = 1
# H = halt = int(6666)

# Input type:
#         transition      input cursor 
#             ↓                ↓ 
# alphabet_a(0a11,0a70)b(0d40)_>input
#          ^                ^
#      _start             action

# Input for unary_add:
# 0+1_a(1a10,+a10,0b01)b(1H00)_>111+110

# Input for language_detector 0^2n
# 01269-_a(0b-0)b(0c20,2b20,-H61)c(0d00,2c20,-e-1)d(0c20,2d20,-H91)e(0e01,2e21,-b-0)_>0000-

alphabet[.,_()>H0123456789abcdef+=-]

_start_mem:
        [any]  <- self then RIGHT and _start_mem
        [>]    <- self then RIGHT and first_read

reach_input_start:
        [any]  <- self then LEFT and reach_input_start
        [_]    <- _ eq ebx int(6666) :true then RIGHT and HALT else RIGHT and reach_cursor

reach_cursor:
        [any]  <- self then RIGHT and reach_cursor
        [>]    <- > eq edx int(0000) :true then RIGHT and read else LEFT and read

read:
        [a]    <- a mov eax int(000a) then LEFT and get_current_state_0
        [b]    <- b mov eax int(000b) then LEFT and get_current_state_0
        [c]    <- c mov eax int(000c) then LEFT and get_current_state_0
        [d]    <- d mov eax int(000d) then LEFT and get_current_state_0
        [e]    <- e mov eax int(000e) then LEFT and get_current_state_0
        [f]    <- f mov eax int(000f) then LEFT and get_current_state_0
        [0]    <- 0 mov eax int(0000) then LEFT and get_current_state_0
        [1]    <- 1 mov eax int(0001) then LEFT and get_current_state_0
        [2]    <- 2 mov eax int(0002) then LEFT and get_current_state_0
        [3]    <- 3 mov eax int(0003) then LEFT and get_current_state_0
        [4]    <- 4 mov eax int(0004) then LEFT and get_current_state_0
        [5]    <- 5 mov eax int(0005) then LEFT and get_current_state_0
        [6]    <- 6 mov eax int(0006) then LEFT and get_current_state_0
        [7]    <- 7 mov eax int(0007) then LEFT and get_current_state_0
        [8]    <- 8 mov eax int(0008) then LEFT and get_current_state_0
        [9]    <- 9 mov eax int(0009) then LEFT and get_current_state_0
        [+]    <- + mov eax int(0010) then LEFT and get_current_state_0
        [=]    <- = mov eax int(0011) then LEFT and get_current_state_0
        [-]    <- - mov eax int(0012) then LEFT and get_current_state_0

get_current_state_0:
        [any]    <- self then LEFT and get_current_state_0
        [_]      <- self then LEFT and get_current_state_1

get_current_state_1:
        [any]    <- self then LEFT  and get_current_state_1
        [_]      <- self then RIGHT and find_state

find_state:
        [a]    <- a eq ebx int(000a) :true then RIGHT and enter_state else RIGHT and skip_to_next_state 
        [b]    <- b eq ebx int(000b) :true then RIGHT and enter_state else RIGHT and skip_to_next_state
        [c]    <- c eq ebx int(000c) :true then RIGHT and enter_state else RIGHT and skip_to_next_state
        [d]    <- d eq ebx int(000d) :true then RIGHT and enter_state else RIGHT and skip_to_next_state
        [e]    <- e eq ebx int(000e) :true then RIGHT and enter_state else RIGHT and skip_to_next_state
        [f]    <- f eq ebx int(000f) :true then RIGHT and enter_state else RIGHT and skip_to_next_state
        [0]    <- 0 eq ebx int(0000) :true then RIGHT and enter_state else RIGHT and skip_to_next_state
        [1]    <- 1 eq ebx int(0001) :true then RIGHT and enter_state else RIGHT and skip_to_next_state
        [2]    <- 2 eq ebx int(0002) :true then RIGHT and enter_state else RIGHT and skip_to_next_state
        [3]    <- 3 eq ebx int(0003) :true then RIGHT and enter_state else RIGHT and skip_to_next_state
        [4]    <- 4 eq ebx int(0004) :true then RIGHT and enter_state else RIGHT and skip_to_next_state
        [5]    <- 5 eq ebx int(0005) :true then RIGHT and enter_state else RIGHT and skip_to_next_state
        [6]    <- 6 eq ebx int(0006) :true then RIGHT and enter_state else RIGHT and skip_to_next_state
        [7]    <- 7 eq ebx int(0007) :true then RIGHT and enter_state else RIGHT and skip_to_next_state
        [8]    <- 8 eq ebx int(0008) :true then RIGHT and enter_state else RIGHT and skip_to_next_state
        [9]    <- 9 eq ebx int(0009) :true then RIGHT and enter_state else RIGHT and skip_to_next_state
        [+]    <- + eq ebx int(0010) :true then RIGHT and enter_state else RIGHT and skip_to_next_state
        [=]    <- = eq ebx int(0011) :true then RIGHT and enter_state else RIGHT and skip_to_next_state
        [-]    <- - eq ebx int(0012) :true then RIGHT and enter_state else RIGHT and skip_to_next_state

skip_to_next_state:
        [any]  <- self then RIGHT and skip_to_next_state
        [)]    <- self then RIGHT and find_state

first_read:
        [a]    <- a mov eax int(000a) then LEFT and get_first_state_0
        [b]    <- b mov eax int(000b) then LEFT and get_first_state_0
        [c]    <- c mov eax int(000c) then LEFT and get_first_state_0
        [d]    <- d mov eax int(000d) then LEFT and get_first_state_0
        [e]    <- e mov eax int(000e) then LEFT and get_first_state_0
        [f]    <- f mov eax int(000f) then LEFT and get_first_state_0
        [0]    <- 0 mov eax int(0000) then LEFT and get_first_state_0
        [1]    <- 1 mov eax int(0001) then LEFT and get_first_state_0
        [2]    <- 2 mov eax int(0002) then LEFT and get_first_state_0
        [3]    <- 3 mov eax int(0003) then LEFT and get_first_state_0
        [4]    <- 4 mov eax int(0004) then LEFT and get_first_state_0
        [5]    <- 5 mov eax int(0005) then LEFT and get_first_state_0
        [6]    <- 6 mov eax int(0006) then LEFT and get_first_state_0
        [7]    <- 7 mov eax int(0007) then LEFT and get_first_state_0
        [8]    <- 8 mov eax int(0008) then LEFT and get_first_state_0
        [9]    <- 9 mov eax int(0009) then LEFT and get_first_state_0
        [+]    <- + mov eax int(0010) then LEFT and get_first_state_0 
        [=]    <- = mov eax int(0011) then LEFT and get_first_state_0 
        [-]    <- - mov eax int(0012) then LEFT and get_first_state_0

get_first_state_0:
        [>]    <- self then LEFT and get_first_state_0
        [_]    <- self then LEFT and get_first_state_1

get_first_state_1:
        [any]  <- self then LEFT  and get_first_state_1
        [_]    <- self then RIGHT and enter_state

enter_state:
        [any]  <- self then RIGHT and enter_state
        [(]    <- self then RIGHT and check_state_read

check_state_read:
        [a]    <- a eq eax int(000a) :true then RIGHT and write_state_1 else RIGHT and get_next_transition 
        [b]    <- b eq eax int(000b) :true then RIGHT and write_state_1 else RIGHT and get_next_transition
        [c]    <- c eq eax int(000c) :true then RIGHT and write_state_1 else RIGHT and get_next_transition
        [d]    <- d eq eax int(000d) :true then RIGHT and write_state_1 else RIGHT and get_next_transition
        [e]    <- e eq eax int(000e) :true then RIGHT and write_state_1 else RIGHT and get_next_transition
        [f]    <- f eq eax int(000f) :true then RIGHT and write_state_1 else RIGHT and get_next_transition
        [0]    <- 0 eq eax int(0000) :true then RIGHT and write_state_1 else RIGHT and get_next_transition
        [1]    <- 1 eq eax int(0001) :true then RIGHT and write_state_1 else RIGHT and get_next_transition
        [2]    <- 2 eq eax int(0002) :true then RIGHT and write_state_1 else RIGHT and get_next_transition
        [3]    <- 3 eq eax int(0003) :true then RIGHT and write_state_1 else RIGHT and get_next_transition
        [4]    <- 4 eq eax int(0004) :true then RIGHT and write_state_1 else RIGHT and get_next_transition
        [5]    <- 5 eq eax int(0005) :true then RIGHT and write_state_1 else RIGHT and get_next_transition
        [6]    <- 6 eq eax int(0006) :true then RIGHT and write_state_1 else RIGHT and get_next_transition
        [7]    <- 7 eq eax int(0007) :true then RIGHT and write_state_1 else RIGHT and get_next_transition
        [8]    <- 8 eq eax int(0008) :true then RIGHT and write_state_1 else RIGHT and get_next_transition
        [9]    <- 9 eq eax int(0009) :true then RIGHT and write_state_1 else RIGHT and get_next_transition
        [+]    <- + eq eax int(0010) :true then RIGHT and write_state_1 else RIGHT and get_next_transition
        [=]    <- = eq eax int(0011) :true then RIGHT and write_state_1 else RIGHT and get_next_transition
        [-]    <- - eq eax int(0012) :true then RIGHT and write_state_1 else RIGHT and get_next_transition

get_next_transition:
        [any]  <- self then RIGHT and get_next_transition
        [,]    <- self then RIGHT and check_state_read

write_state_1:
        [a]    <- a mov ebx int(000a) then RIGHT and write_state_2
        [b]    <- b mov ebx int(000b) then RIGHT and write_state_2
        [c]    <- c mov ebx int(000c) then RIGHT and write_state_2
        [d]    <- d mov ebx int(000d) then RIGHT and write_state_2
        [e]    <- e mov ebx int(000e) then RIGHT and write_state_2
        [f]    <- f mov ebx int(000f) then RIGHT and write_state_2
        [0]    <- 0 mov ebx int(0000) then RIGHT and write_state_2
        [1]    <- 1 mov ebx int(0001) then RIGHT and write_state_2
        [2]    <- 2 mov ebx int(0002) then RIGHT and write_state_2
        [3]    <- 3 mov ebx int(0003) then RIGHT and write_state_2
        [4]    <- 4 mov ebx int(0004) then RIGHT and write_state_2
        [5]    <- 5 mov ebx int(0005) then RIGHT and write_state_2
        [6]    <- 6 mov ebx int(0006) then RIGHT and write_state_2
        [7]    <- 7 mov ebx int(0007) then RIGHT and write_state_2
        [8]    <- 8 mov ebx int(0008) then RIGHT and write_state_2
        [9]    <- 9 mov ebx int(0009) then RIGHT and write_state_2
        [+]    <- + mov ebx int(0010) then RIGHT and write_state_2
        [=]    <- = mov ebx int(0011) then RIGHT and write_state_2
        [-]    <- - mov ebx int(0012) then RIGHT and write_state_2
        [H]    <- H mov ebx int(6666) then RIGHT and write_state_2

write_state_2:
        [a]    <- a mov ecx int(000a) then RIGHT and write_state_3
        [b]    <- b mov ecx int(000b) then RIGHT and write_state_3
        [c]    <- c mov ecx int(000c) then RIGHT and write_state_3
        [d]    <- d mov ecx int(000d) then RIGHT and write_state_3
        [e]    <- e mov ecx int(000e) then RIGHT and write_state_3
        [f]    <- f mov ecx int(000f) then RIGHT and write_state_3
        [0]    <- 0 mov ecx int(0000) then RIGHT and write_state_3
        [1]    <- 1 mov ecx int(0001) then RIGHT and write_state_3
        [2]    <- 2 mov ecx int(0002) then RIGHT and write_state_3
        [3]    <- 3 mov ecx int(0003) then RIGHT and write_state_3
        [4]    <- 4 mov ecx int(0004) then RIGHT and write_state_3
        [5]    <- 5 mov ecx int(0005) then RIGHT and write_state_3
        [6]    <- 6 mov ecx int(0006) then RIGHT and write_state_3
        [7]    <- 7 mov ecx int(0007) then RIGHT and write_state_3
        [8]    <- 8 mov ecx int(0008) then RIGHT and write_state_3
        [9]    <- 9 mov ecx int(0009) then RIGHT and write_state_3
        [+]    <- + mov ecx int(0010) then RIGHT and write_state_3
        [=]    <- = mov ecx int(0011) then RIGHT and write_state_3
        [-]    <- - mov ecx int(0012) then RIGHT and write_state_3

write_state_3:
        [0]    <- 0 mov edx int(0000) then RIGHT and goto_cursor
        [1]    <- 1 mov edx int(0001) then RIGHT and goto_cursor

goto_cursor:
        [any]  <- self then RIGHT and goto_cursor
        [>]    <- > eq edx int(0000) :true then RIGHT and overwrite_cursor_right_start else LEFT and overwrite_cursor_left_start

overwrite_cursor_left_start:
        [any] <- > then RIGHT and try_overwrite_cursor_0

overwrite_cursor_right_start:
        [any] <- > then LEFT  and try_overwrite_cursor_0

try_overwrite_cursor_0:
        [>]    <- > eq ecx int(0000) :true then LEFT and overwrite_cursor_0_0 else LEFT and wait_1

overwrite_cursor_0_0:
        [any] <- self then RIGHT and overwrite_cursor_0_1

overwrite_cursor_0_1:
        [>] <- 0 then LEFT and reach_input_start
wait_1:
        [any] <- self then RIGHT and try_overwrite_cursor_1

try_overwrite_cursor_1:
        [>]    <- > eq ecx int(0001) :true then LEFT and overwrite_cursor_1_0 else LEFT and wait_2

overwrite_cursor_1_0:
        [any] <- self then RIGHT and overwrite_cursor_1_1

overwrite_cursor_1_1:
        [>] <- 1 then LEFT and reach_input_start
wait_2:
        [any] <- self then RIGHT and try_overwrite_cursor_2

try_overwrite_cursor_2:
        [>]    <- > eq ecx int(0002) :true then LEFT and overwrite_cursor_2_0 else LEFT and wait_3

overwrite_cursor_2_0:
        [any] <- self then RIGHT and overwrite_cursor_2_1

overwrite_cursor_2_1:
        [>] <- 2 then LEFT and reach_input_start

wait_3:
        [any] <- self then RIGHT and try_overwrite_cursor_3

try_overwrite_cursor_3:
        [any]    <- > eq ecx int(0003) :true then LEFT and overwrite_cursor_3_0 else LEFT and wait_4

overwrite_cursor_3_0:
        [any] <- self then RIGHT and overwrite_cursor_3_1

overwrite_cursor_3_1:
        [>] <- 2 then LEFT and reach_input_start

wait_4:
        [any] <- self then RIGHT and try_overwrite_cursor_4

try_overwrite_cursor_4:
        [>]    <- > eq ecx int(0004) :true then LEFT and overwrite_cursor_4_0 else LEFT and wait_5

overwrite_cursor_4_0:
        [any] <- self then RIGHT and overwrite_cursor_4_1

overwrite_cursor_4_1:
        [>] <- 4 then LEFT and reach_input_start

wait_5:
        [any] <- self then RIGHT and try_overwrite_cursor_5

try_overwrite_cursor_5:
        [any]    <- > eq ecx int(0005) :true then LEFT and overwrite_cursor_5_0 else LEFT and wait_6

overwrite_cursor_5_0:
        [any] <- self then RIGHT and overwrite_cursor_5_1

overwrite_cursor_5_1:
        [>] <- 5 then LEFT and reach_input_start

wait_6:
        [any] <- self then RIGHT and try_overwrite_cursor_6

try_overwrite_cursor_6:
        [>]    <- > eq ecx int(0006) :true then LEFT and overwrite_cursor_6_0 else LEFT and wait_7

overwrite_cursor_6_0:
        [any] <- self then RIGHT and overwrite_cursor_6_1

overwrite_cursor_6_1:
        [>] <- 6 then LEFT and reach_input_start

wait_7:
        [any] <- self then RIGHT and try_overwrite_cursor_7

try_overwrite_cursor_7:
        [any]    <- > eq ecx int(0007) :true then LEFT and overwrite_cursor_7_0 else LEFT and wait_8

overwrite_cursor_7_0:
        [any] <- self then RIGHT and overwrite_cursor_7_1

overwrite_cursor_7_1:
        [>] <- 7 then LEFT and reach_input_start

wait_8:
        [any] <- self then RIGHT and try_overwrite_cursor_8

try_overwrite_cursor_8:
        [>]    <- > eq ecx int(0008) :true then LEFT and overwrite_cursor_8_0 else LEFT and wait_9

overwrite_cursor_8_0:
        [>] <- > then RIGHT and overwrite_cursor_8_1

overwrite_cursor_8_1:
        [any] <- 8 then LEFT and reach_input_start

wait_9:
        [any] <- self then RIGHT and try_overwrite_cursor_9

try_overwrite_cursor_9:
        [>]    <- > eq ecx int(0009) :true then LEFT and overwrite_cursor_9_0 else LEFT and wait_a

overwrite_cursor_9_0:
        [any] <- self then RIGHT and overwrite_cursor_9_1

overwrite_cursor_9_1:
        [>] <- 9 then LEFT and reach_input_start

wait_a:
        [any] <- self then RIGHT and try_overwrite_cursor_a

try_overwrite_cursor_a:
        [>]    <- > eq ecx int(000a) :true then LEFT and overwrite_cursor_a_0 else LEFT and wait_b

overwrite_cursor_a_0:
        [any] <- self then RIGHT and overwrite_cursor_a_1

overwrite_cursor_a_1:
        [any] <- a then LEFT and reach_input_start

wait_b:
        [any] <- self then RIGHT and try_overwrite_cursor_b

try_overwrite_cursor_b:
        [any]    <- > eq ecx int(000b) :true then LEFT and overwrite_cursor_b_0 else LEFT and wait_c

overwrite_cursor_b_0:
        [any] <- self then RIGHT and overwrite_cursor_b_1

overwrite_cursor_b_1:
        [>] <- b then LEFT and reach_input_start

wait_c:
        [any] <- self then RIGHT and try_overwrite_cursor_c

try_overwrite_cursor_c:
        [>]    <- > eq ecx int(000c) :true then LEFT and overwrite_cursor_c_0 else LEFT and wait_d

overwrite_cursor_c_0:
        [any] <- self then RIGHT and overwrite_cursor_c_1

overwrite_cursor_c_1:
        [any] <- c then LEFT and reach_input_start

wait_d:
        [any] <- self then RIGHT and try_overwrite_cursor_d

try_overwrite_cursor_d:
        [any]    <- > eq ecx int(000d) :true then LEFT and overwrite_cursor_d_0 else LEFT and wait_e

overwrite_cursor_d_0:
        [any] <- self then RIGHT and overwrite_cursor_d_1

overwrite_cursor_d_1:
        [>] <- d then LEFT and reach_input_start

wait_e:
        [any] <- self then RIGHT and try_overwrite_cursor_e

try_overwrite_cursor_e:
        [>]    <- > eq ecx int(000e) :true then LEFT and overwrite_cursor_e_0 else LEFT and wait_f

overwrite_cursor_e_0:
        [any] <- self then RIGHT and overwrite_cursor_e_1

overwrite_cursor_e_1:
        [any] <- e then LEFT and reach_input_start

wait_f:
        [any] <- self then RIGHT and try_overwrite_cursor_f

try_overwrite_cursor_f:
        [>]    <- > eq ecx int(000f) :true then LEFT and overwrite_cursor_f_0 else LEFT and wait_plus

overwrite_cursor_f_0:
        [any] <- self then RIGHT and overwrite_cursor_f_1

overwrite_cursor_f_1:
        [>] <- f then LEFT and reach_input_start

wait_plus:
        [any] <- self then RIGHT and try_overwrite_cursor_plus

try_overwrite_cursor_plus:
        [>]    <- > eq ecx int(0010) :true then LEFT and overwrite_cursor_plus_0 else LEFT and wait_equal

overwrite_cursor_plus_0:
        [any] <- self then RIGHT and overwrite_cursor_plus_1

overwrite_cursor_plus_1:
        [>] <- + then LEFT and reach_input_start

wait_equal:
        [any] <- self then RIGHT and try_overwrite_cursor_equal

try_overwrite_cursor_equal:
        [>]    <- > eq ecx int(0011) :true then LEFT and overwrite_cursor_equal_0 else LEFT and wait_minus

overwrite_cursor_equal_0:
        [any] <- self then RIGHT and overwrite_cursor_4_1

overwrite_cursor_equal_1:
        [>] <- 4 then LEFT and reach_input_start

wait_minus:
        [any] <- self then RIGHT and try_overwrite_cursor_minus

try_overwrite_cursor_minus:
        [>]    <- > eq ecx int(0012) :true then LEFT and overwrite_cursor_minus_0 else LEFT and out_of_bound_alphabet

overwrite_cursor_minus_0:
        [any] <- self then RIGHT and overwrite_cursor_minus_1

overwrite_cursor_minus_1:
        [>] <- - then LEFT and reach_input_start

out_of_bound_alphabet:
        [any] <- self then LEFT and ERROR
