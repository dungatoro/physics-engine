module verlet
    implicit none
    real, dimension(2) :: g = 9.81

    type body
        real, dimension(2) :: pos, v, a
        real :: m, drag
    end type
contains
    subroutine update(o, dt)
        ! updates a body's position, velocity and acceleration with time
        type(body), intent(inout) :: o
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
        ! calculates new acceleration due to forces acting on the body
        type(body), intent(in) :: o
        real, dimension(2) :: f_drag 
        real, dimension(2) :: a_drag
        real, dimension(2) :: with_forces
    
        a_drag = f_drag / o%m
        f_drag = 0.5 * o%drag * o%v**2
        with_forces = g - a_drag
    end function
end module

program main
    use verlet
    implicit none
end program

