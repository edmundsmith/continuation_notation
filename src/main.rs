#![feature(custom_attribute)]
extern crate continue_notation_macro;

use continue_notation_macro::use_cps;

fn main() {
    
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        println!("It works!");
    }

    
}

    #[use_cps]
    #[test]
    fn cps_test() {
        println!("Initial");
        fn cps<F,R>(x:i32,mut f:F) -> R where F:FnMut(i32) -> R {
            f(x)
        }
        let mut i = 0;
        cont!(cps,(5,_) as x);
        i+=1;
        assert!(i==1);
        println!("x={}",x);
        {
            cont!(cps, (x+2, _) as y);
            i+=1;
            assert!(i==2);
            println!("y = {} = {} + 2 = x + 2", y, x);
        }
        i+=1;
        assert!(i==3);
    }