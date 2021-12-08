# Introduction to Fortran
## Basic syntax
- **`hello_world.f90`**
  ```fortran
  program program_name
    ! comment line
    print *, 'Hello, world!'
  end program program_name
  ```
- Compile
  - `gfortran hello_world.f90 -o hello_world`
  - https://gcc.gnu.org/fortran/
- [rules](https://www.ibm.com/docs/en/xl-fortran-aix/16.1.0?topic=fundamentals-names) for `program_name`
  - Letters (A-Z, a-z)
  - Digits (0-9)
  - Underscores (_)
  - The first character of a name must not be a digit. In Fortran 2003, the maximum length of a name is 63 characters. In Fortran 90 and Fortran 95, the maximum length of a name is 31 characters.
- Subroutine
  ```fortran
  subroutine sub_program (var_name)      
    implicit none
    integer :: var_name
  end subroutine sub_program
  ```
  - `call sub_program (var_name)`

## Data types
- There are 5 intrinsic data types: `integer`, `real`, `complex`, `logical`, `character`
- Variable declaration
  - **`variable_type :: variable_name`**
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
  program read_var
    implicit none
    integer :: val

    print *, 'Please enter an integer: '
    read(*,*) val

    print *, val
  end program read_var
  ```

- Floating-point precision (for **Fortran 2003** or later)
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

- [character functions](https://www.tutorialspoint.com/fortran/fortran_characters.htm)

## Variables & Constants
- `#define PI 3.141592`
- `#include "define.h"` (defined variables are written in `define.h`)
- **`data_type, parameter :: variable = val`**
  ```fortran
  program free_falling
    implicit none
    real, parameter :: g = 9.81
    real :: t

    t = 3.0623822
    print *, g*t**2/2 
  end program free_falling
  ```

## Array 
- Definition
  - `integer, dimension(3) :: array`
  - `integer :: array(3)`
  - `real, dimension(10, 10) :: array2d`
  - custom index: `real :: array4(0:6)`, `real :: array5(-3:3)`
  - `integer, allocatable :: array6(:)` (see **Dynamic array**)
- Static array
  ```fortran
  program arrays
    implicit none

    integer :: i, j
    integer :: array1(10), array2(5)
    integer :: array3(3, 3)

    array1 = (/1, 2, 3, 4, 5, 6, 7, 8, 9, 10 /) ! array constructor
    array1 = (/(i, i = 1, 10)/) ! do loop constructor
    array2(:) = 0
    array2(3:5) = 1
    do i = 1,3
        do j = 1,3
           array3(i, j) = i+j
        end do
     end do

    print *, array1(1:10:2) ! even index
    print *, array2(5:1:-1) ! reverse
    print *, array3(:,1) ! column 1

  end program arrays
  ```
- Dynamic array
  ```fortran
  program dynamic_array
    implicit none
    integer :: val
    integer, allocatable :: array1(:)

    print *, 'Please enter an integer: '
    read(*,*) val
    print *, "Initiate a 1D array of ", val, " elements and set all to 1."

    allocate(array1(val))
    array1 = 1
    print *, array1
    deallocate(array1)

  end program dynamic_array  
  ```

## String
- Definition
  - `character(len = 10) :: string1`
  - `character(:), allocatable :: string2` (see **Dynamic string**)
- Static string
  ```fortran
  program string
  implicit none

     character(len = 10) :: ind
     character(len = 1) :: a, d 
     character(len = 6) :: b
     character(len = 2) :: c

     ind = '1234567890'
     a = 'ab'
     b = 'cde'
     c = ' f'
     d = '.'

     print *, ind
     print *, ind(3:5)  ! substring
     print *, a, b, c, d
     ind = a//b//c//d   ! string concatenation
     print *, ind
     print *, a, trim(b), c, d   ! trim trailing blanks
     print *, a, adjustr(b), adjustl(c), d   ! adjust string to the right/left

     ! search substring in a string
     d = 'd'
     if( index(b, d) == 0 ) then
        print *, 'Not found!'
     else
        print *, 'Found at index: ', index(b, d)
     end if

  end program string
  ```
- Dynamic string
  - The length of allocatable string can be varied during the program runtime. (see [ref.](https://fortranwiki.org/fortran/files/character_handling_in_Fortran.html))

## Operators and flow control
- arithmetic Operators
- relational Operators
- logical Operators (`.and.`, `.or.`, `.not.`, `.eqv.`, `.neqv.`)
- **if-else construct**
  ```fortran
  program ifelse
  implicit none
     integer :: a = 1
     integer :: b = 1

     if( a == 1 ) then
        print *, a
        if( a == b ) then
          print*, a==b
        end if
     else if( a == 2 ) then
        print *, "a is not 2."
     else
        print *, "What is a?" 
     end if
  end program ifelse
  ```
- **select-case construct**
  ```fortran
  program select_case
  implicit none
     character(len=7) :: option = 'on'
     integer :: i = 1

     select case (option) 
        case ('on') 
        print *, "Turn on!"

        select case (i)
          case (4:6, 8:10)
          print *, "4<=i<=6 or 8<=i<=10"

          case (:3)
          print *, "i<=3"
        end select

          case ('off')
          print *, "Turn off!"

          case ('nothing') 
             print *, "Do nothing!" 
       end select
    end program select_case
   ```
- **loop construct**
  ```fortran
  program loop  
  implicit none  
     integer :: nfact = 1   
     integer :: n, i=1, s=0 

     ! compute factorials   
     do n = 1, 10      
        nfact = nfact * n 
        print*,  n, " ", nfact   
     end do

     ! compute summation
      do while (i < 11)
        i = i + 1
        if (mod(i, 2) == 0) then
          cycle ! jump to the next loop
        else if (i==9) then
          exit ! exit the loop
        end if
        s = s + i
        print *, i, s
      end do

  end program loop 
  ```
  - control statement: `cycle`, `exit`, `stop`
  - parallelizable loop: `do concurrent`

## Resources
- Online IDE
  - https://www.tutorialspoint.com/compile_fortran_online.php
  - https://www.onlinegdb.com/

## References
- https://www.tutorialspoint.com/fortran/index.htm
- https://fortran-lang.org/learn/
