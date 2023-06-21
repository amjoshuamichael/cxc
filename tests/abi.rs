mod test_utils;
use test_utils::xc_test;

#[test]
fn pass_8_byte_sep() {
    xc_test!(
        "
        sum(of: {i32, i32}); i32 { ; of.0 + of.1 }

        main(); i32 {
            x: {i32, i32} = { 43, 65 }
            sum_x = sum(x)
            ; sum_x
        }
        ";
        43 + 65
    );
}

//TODO
#[test]
#[ignore]
fn pass_12_byte_2sep() {
    xc_test!(
        "
        sum(of: {i64, i32}); i64 { ; cast<i32, i64>(of.1) }

        main(); i64 {
            # a little over the 32 bit integer limit, to make sure this is *really* an i64
            x: {i64, i32} = { i64 2_147_483_694, 65, }
            sum_x = sum(x)
            ; sum_x
        }
        ";
        2_147_483_694i64 + 65i64
    );
}

#[test]
fn pass_12_byte_3sep() {
    xc_test!(
        "
        sum(of: {i32, i32, i32}); i32 { ; of.0 + of.1 + of.2 }

        main(); i32 {
            x: {i32, i32, i32} = { 43, 65, 4 }
            sum_x = sum(x)
            ; sum_x
        }
        ";
        43 + 65 + 4
    );
}

#[test]
fn pass_16_byte_2sep() {
    xc_test!(
        "
        sum(of: {i64, i64}); i64 { ; of.0 + of.1 }

        main(); i64 {
            # a little over the 32 bit integer limit, to make sure this is *really* an i64
            x: {i64, i64} = { i64 2_147_483_694, i64 65, }
            sum_x = sum(x)
            ; sum_x
        }
        ";
        2_147_483_694i64 + 65i64
    );
}
