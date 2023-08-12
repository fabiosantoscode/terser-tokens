static BASE54: [char; 54] = [
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's',
    't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L',
    'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '$', '_',
];

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Base54(usize);

impl Base54 {
    pub fn new(num: usize) -> Self {
        Self(num)
    }

    pub fn next(&self) -> Self {
        Self(self.0 + 1)
    }
}

impl ToString for Base54 {
    fn to_string(&self) -> String {
        let mut num = self.0;
        if num < 54 {
            BASE54[num].to_string()
        } else {
            let mut result = String::new();

            while num >= 54 {
                let rem = num % 54;
                result.push(BASE54[rem]);
                num = num / 54;
            }

            result.push(BASE54[num]);
            result.chars().rev().collect::<String>()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_base54() {
        assert_eq!(Base54(0).to_string(), "a");
        assert_eq!(Base54(1).to_string(), "b");
        assert_eq!(Base54(2).to_string(), "c");

        assert_eq!(Base54(53).to_string(), "_");
        assert_eq!(Base54(54).to_string(), "ba");

        assert_eq!(Base54(54 + 53).to_string(), "b_");
        assert_eq!(Base54(54 + 54).to_string(), "ca");
    }
}
