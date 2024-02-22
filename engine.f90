module ball_object
    implicit none
    real, dimension(2) :: g = 9.81

    type ball
        real, dimension(2) :: pos, v, a
        real :: r, m, drag = 0.47
    end type
contains
    subroutine update(o, dt)
        ! updates a ball's position, velocity and acceleration with time
        type(ball), intent(inout) :: o
        real, intent(in) :: dt
        real, dimension(2) :: new_pos, new_v, new_a
    
        new_pos = o%pos + o%v*dt + o%a*(dt*dt*0.5)
        new_a   = with_forces(o)
        new_v   = o%v + (o%a+new_a)*(dt*0.5)
    
        o%pos = new_pos
        o%v   = new_v
        o%a   = new_a
    end subroutine
    
    function with_forces(o) 
        ! calculates new acceleration due to forces acting on the ball
        type(ball), intent(in) :: o
        real, dimension(2) :: f_drag 
        real, dimension(2) :: a_drag
        real, dimension(2) :: with_forces
    
        a_drag = f_drag / o%m
        f_drag = 0.5 * o%drag * o%v**2
        with_forces = g - a_drag
    end function

    ! TODO
    ! - Check overlapping -> Calculate push force
    ! - Write to screen

end module

program main
    use ball_object
    implicit none

    integer :: i
    real :: r = 1.0, m = 1.0
    real, dimension(2) :: pos, v, a
    type(ball) :: balls(100)

    do i = 1, 100
        call random_number(pos)
        call random_number(v)
        call random_number(a)
        balls(i) = ball(pos, v, a, r, m)
    end do

    print *, balls
end program

