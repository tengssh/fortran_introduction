# Introduction to Fortran
## Basic syntax
- `hello_world.f90`
  ```fortran
  program program_name
    ! comment line
    print *, 'Hello, world!'
  end program program_name
  ```
- compile
  - `gfortran hello_world.f90 -o hello_world`
  - https://gcc.gnu.org/fortran/

## Data types
- There are 5 intrinsic data types: `integer`, `real`, `complex`, `logical`, `character`
- Variable declaration
  - `variable_type :: variable_name`
    ```fortran
    program variable
    implicit none ! This statement turns off implicit typing (good practice in programming).
      integer :: a
      integer(kind = 2) :: b ! n = 2, 4(default), 8, 16
      real :: c
      complex :: d
      character(len=30) :: e
      logical :: f
      a=0
      b=1
      c=0.1
      d=(0.1, -0.1)
      e='string' ! e(1:3) is 'str'
      f=.true.
      print *, huge(a), kind(a)
      print *, huge(b), kind(b)
      print *, c, kind(c)
      print *, d, kind(d)
      print *, cmplx(a,b)
      print *, e, kind(e)
      print *, f, kind(f)
    end program variable
    ```
- Read variables
  ```fortran
  program read
    implicit none
    integer :: val

    print *, 'Please enter an integer: '
    read(*,*) val

    print *, val
  end program read
  ```

- Floating-point precision
```fortran
program float
  use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
  implicit none
  real(sp) :: f32
  real(dp) :: f64

  f32 = 1.0_sp
  f64 = 1.0_dp
  
  print *, f32, kind(f32)
  print *, f64, kind(f64)
end program float
```

## Variables
- `#define PI 3.141592`
- `#include "define.h"` (defined variables are written in `define.h`)

## References
- https://www.tutorialspoint.com/fortran/index.htm
- https://fortran-lang.org/learn/
