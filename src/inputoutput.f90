module inputoutput_m
  use type_dec
  use constants_m

contains


!============================================================================================================
!============================================================================================================

  subroutine readbspvar(bsp, mbsp, CP, namebsp)

    implicit none
    integer :: mbsp, i, j, k, n_CP, namepos
    type(bsp_t), dimension (:), allocatable, target :: bsp
    character (100) :: namebsp, nametrim
    type(cpts_t), dimension(:), allocatable :: CP
    

    nametrim=trim(namebsp)

    open(66, file=nametrim)

    read(66,*) mbsp
    allocate(bsp(mbsp))
    do i=1,mbsp
       read(66,*) bsp(i)%mctl
       read(66,*) bsp(i)%p
       read(66,*) bsp(i)%mknot
       allocate(bsp(i)%knot(0:bsp(i)%mknot))
       read(66,*) (bsp(i)%knot(j), j=0,bsp(i)%mknot)
       allocate(bsp(i)%cpts_id(0:bsp(i)%mctl))
       read(66,*) (bsp(i)%cpts_id(j), j=0,bsp(i)%mctl)
    end do

    read(66,*) n_CP
    allocate(CP(n_CP))
    do i=1,n_CP
       read(66,*) CP(i)%x
    end do


    close(66)

    do i=1,mbsp
       allocate(bsp(i)%alpha(0:bsp(i)%mctl))
       do j=0,bsp(i)%mctl
          bsp(i)%alpha(j)%x = CP(bsp(i)%cpts_id(j))%x
       end do
    end do


  end subroutine readbspvar



end module inputoutput_m
