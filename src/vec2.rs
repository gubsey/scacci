use std::{
    iter::{Successors, successors},
    ops::{Add, AddAssign, Mul, MulAssign, Sub, SubAssign},
    str::FromStr,
};

#[derive(Default, Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Vec2<T = i32> {
    pub x: T,
    pub y: T,
}

impl<T> Vec2<T> {
    pub fn transposed(mut self) -> Self {
        std::mem::swap(&mut self.x, &mut self.y);
        self
    }
}

impl Vec2 {
    pub fn to_usize(self) -> Vec2<usize> {
        Vec2::<usize> {
            x: self.x as usize,
            y: self.y as usize,
        }
    }

    pub fn to_fen(self) -> String {
        String::from_utf8(vec![self.x as u8 + b'a', self.y as u8 + b'1']).unwrap()
    }

    pub fn flipedy(mut self) -> Self {
        self.y *= -1;
        self
    }
    pub fn rotate90(&mut self) {
        *self = self.rotated90()
    }
    pub fn rotated90(self) -> Self {
        self.transposed().flipedy()
    }
    pub fn cardinals(self) -> Successors<Vec2, impl FnMut(&Vec2) -> Option<Vec2>> {
        successors(Some(self), |x| Some(x.rotated90()))
    }
    pub fn in_bounds(&self) -> bool {
        matches!(self.x, 0..8) && matches!(self.y, 0..8)
    }
    pub fn abs(mut self) -> Self {
        self.x = self.x.abs();
        self.y = self.y.abs();
        self
    }
}

impl<T: AddAssign> Add for Vec2<T> {
    type Output = Self;
    fn add(self, mut rhs: Self) -> Self::Output {
        rhs.x += self.x;
        rhs.y += self.y;
        rhs
    }
}
impl<T: AddAssign + Copy> Add<T> for Vec2<T> {
    type Output = Self;
    fn add(mut self, rhs: T) -> Self::Output {
        self.x += rhs;
        self.y += rhs;
        self
    }
}
impl<T: AddAssign> AddAssign for Vec2<T> {
    fn add_assign(&mut self, rhs: Self) {
        self.x += rhs.x;
        self.y += rhs.y;
    }
}
impl<T: SubAssign> Sub for Vec2<T> {
    type Output = Self;
    fn sub(mut self, rhs: Self) -> Self::Output {
        self.x -= rhs.x;
        self.y -= rhs.y;
        self
    }
}
impl<T: SubAssign> SubAssign for Vec2<T> {
    fn sub_assign(&mut self, rhs: Self) {
        self.x -= rhs.x;
        self.y -= rhs.y;
    }
}
impl<T: MulAssign> Mul for Vec2<T> {
    type Output = Self;
    fn mul(mut self, rhs: Self) -> Self::Output {
        self.x *= rhs.x;
        self.y *= rhs.y;
        self
    }
}
impl<T: MulAssign + Copy> Mul<T> for Vec2<T> {
    type Output = Self;
    fn mul(mut self, rhs: T) -> Self::Output {
        self.x *= rhs;
        self.y *= rhs;
        self
    }
}
pub fn xy<T>(x: T, y: T) -> Vec2<T> {
    Vec2 { x, y }
}

impl FromStr for Vec2 {
    type Err = &'static str;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let chars: Vec<char> = s.chars().collect();
        if chars.len() != 2 {
            return Err("string not exactly 2 chars");
        }
        let x = match chars[0] {
            c @ 'a'..='h' => c as i32 - 'a' as i32,
            _ => return Err("first char out of bounds"),
        };
        let y = match chars[1].to_digit(10).ok_or("second char not a number")? {
            y @ 1..=8 => y as i32 - 1,
            _ => return Err("second char not in range"),
        };
        Ok(Self { x, y })
    }
}
