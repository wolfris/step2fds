module eval_nurbs_m
  use type_dec
  use nurbs_m

contains

!============================================================================================================
!============================================================================================================

  subroutine eval_curve(bsp, mbsp, alpha_g, res_comp_u)
    
    integer :: k, nbsp, i, mbsp, control, m, p, res_comp_u
    integer, dimension(:), pointer :: cpts_id
    real(wp) :: upper, lower, incre_u
    real(wp), dimension (:), pointer :: u
    real(wp), dimension(res_comp_u) :: u_pts
    real(wp), dimension (:), allocatable :: ckx, cky, ckz
    type(bsp_t), dimension (:), allocatable, target :: bsp
    type(cpts_t), dimension(:), allocatable :: alpha_g
    type(point_t), dimension(:), allocatable :: Ctl_p

    do nbsp = 1, mbsp
       control = bsp(nbsp)%mctl
       m = bsp(nbsp)%mknot
       u => bsp(nbsp)%knot
       p = bsp(nbsp)%p
       cpts_id => bsp(nbsp)%cpts_id
       allocate (Ctl_p(0:control))

       

       do k = 0, control
          Ctl_p(k)%x = alpha_g(cpts_id(k))%x
       enddo



       upper = -1.e6
       lower = 1.e6	
       do i = 0, m
          if (u(i) < lower) lower = u(i)
          if (u(i) > upper) upper = u(i)
       enddo
       incre_u = (upper - lower) / (res_comp_u - 1)

       do i = 0, res_comp_u-1
          u_pts(i+1) = lower  + i * incre_u
       enddo

       u_pts(res_comp_u) = u(m)


       if (allocated(bsp(nbsp)%surf_pts)) deallocate(bsp(nbsp)%surf_pts)
       allocate (bsp(nbsp)%surf_pts(res_comp_u))
       if (allocated(bsp(nbsp)%surf_grad)) deallocate(bsp(nbsp)%surf_grad)
       allocate (bsp(nbsp)%surf_grad(res_comp_u))
       if (allocated(bsp(nbsp)%surf_sec_dev)) deallocate(bsp(nbsp)%surf_sec_dev)
       allocate (bsp(nbsp)%surf_sec_dev(res_comp_u))


       if(allocated(ckx)) deallocate(ckx)
       allocate(ckx(0:p))
       if(allocated(cky)) deallocate(cky)
       allocate(cky(0:p))
       if(allocated(ckz)) deallocate(ckz)
       allocate(ckz(0:p))
       do i = 1, res_comp_u
          if (nbsp.eq.236) then
             call curvederivsalg1(Ctl_p%x(1), Ctl_p%x(2), Ctl_p%x(3), p, control, u, u_pts(i), 2, ckx, cky, ckz)
             bsp(nbsp)%surf_pts(i)%x(1) =  ckx(0)
             bsp(nbsp)%surf_pts(i)%x(2) =  cky(0)
             bsp(nbsp)%surf_pts(i)%x(3) =  ckz(0)
             bsp(nbsp)%surf_grad(i)%x(1) =  ckx(1)
             bsp(nbsp)%surf_grad(i)%x(2) =  cky(1)
             bsp(nbsp)%surf_grad(i)%x(3) =  ckz(1)
             bsp(nbsp)%surf_sec_dev(i)%x(1) =  ckx(2)
             bsp(nbsp)%surf_sec_dev(i)%x(2) =  cky(2)
             bsp(nbsp)%surf_sec_dev(i)%x(3) =  ckz(2)
          else
             call curvederivsalg1(Ctl_p%x(1), Ctl_p%x(2), Ctl_p%x(3), p, control, u, u_pts(i), 1, ckx, cky, ckz)
             bsp(nbsp)%surf_pts(i)%x(1) =  ckx(0)
             bsp(nbsp)%surf_pts(i)%x(2) =  cky(0)
             bsp(nbsp)%surf_pts(i)%x(3) =  ckz(0)
             bsp(nbsp)%surf_grad(i)%x(1) =  ckx(1)
             bsp(nbsp)%surf_grad(i)%x(2) =  cky(1)
             bsp(nbsp)%surf_grad(i)%x(3) =  ckz(1)
          end if
       enddo


       deallocate(Ctl_p)
    end do

    
    
  end subroutine eval_curve


!============================================================================================================
!============================================================================================================





end module eval_nurbs_m


