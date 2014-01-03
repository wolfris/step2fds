Program main
  use constants_m
  use type_dec
  use inputoutput_m
  use nurbs_m
  use eval_nurbs_m

  call comp_splines()

Contains


  subroutine comp_splines()
    
    implicit none

    character(len=100) :: name_spline 
    integer :: mbsp, k, i, j, res_u
    type(bsp_t), dimension (:), allocatable :: splines
    type(cpts_t), dimension(:), allocatable :: CP


!============================================================================================================================
!============================================================================================================================

    read(*,'(A)') name_spline


    call readbspvar(splines, mbsp, CP, name_spline)

    
    do i=1,size(CP)
       CP(i)%x(1) = CP(i)%x(1) + 32.0d0 - 1680.0d0
!       CP(i)%x(1) = CP(i)%x(1) + 32.0d0 - 1682.05d0
!       CP(i)%x(1) = CP(i)%x(1) + 32.0d0 - 1678.55d0
!       CP(i)%x(2) = CP(i)%x(2) - 149.64d0
       CP(i)%x(2) = CP(i)%x(2) - 159.3d0
!       CP(i)%x(2) = CP(i)%x(2) - 88.37d0
!       CP(i)%x(2) = CP(i)%x(2) - 220.7d0
    end do

    
    res_u = 400
    call eval_curve(splines, mbsp, CP, res_u)

    

    do i=1,mbsp
       do j=1,res_u
          write(*,'(F18.11,A,F18.11)') splines(i)%surf_pts(j)%x(1),',', splines(i)%surf_pts(j)%x(2) 
       end do
    end do

    stop
    
    
    
  end subroutine comp_splines
  
end Program Main

