divide_by_two (num ) {
    output = num / 2.0
    ! output
}

mul_by_two (num) {
    output = num * 2
    ! output
}
    
main () {
    correct_count = 0
    
    six_times_two = mul_by_two(6)
    ? six_times_two == 12 {
        correct_count = correct_count + 1
    }

    six_div_two_f = divide_by_two(6.0)
    ? six_div_two_f == 3.0 {
        correct_count = correct_count + 1
    }

    ! correct_count
}
