module nurbs_m
  use type_dec


contains

!============================================================================================================
!============================================================================================================
 
  integer function findspan(u_point, p, control, u)
    implicit none
    integer :: p, control
    real(wp) :: u_point
    real(wp), dimension(0:) :: u
    integer :: low, high, mid


    if (abs(u_point - u(control+1)) < small) then
       findspan = control
       return
    elseif(abs(u_point - u(p)) < small) then
       findspan = p
       return
    endif
    
    low = p
    high = control + 1
    mid = (low+high)/2

    
    do while (u_point .lt. u(mid) .or. u_point .ge. u(mid+1)) 
       if (u_point .lt. u(mid)) then
          high = mid
       else
          low = mid
       end if
       mid = (low+high)/2
    end do
    findspan = mid
  end function findspan

  
!============================================================================================================
!============================================================================================================


  subroutine basisfuncs(i, u_point, p, u, n)
    implicit none
    integer :: i, p
    real(wp) :: u_point
    real(wp), dimension(0:) :: u
    real(wp), dimension(0:) :: n
    integer :: j, r
    real(wp), dimension(p) :: left, right
    real(wp) :: saved, temp
    
    n(0) = 1.0
    do j=1,p
       left(j) = u_point - u(i+1-j)
       right(j) = u(i+j) - u_point
       saved = 0.0
       do r=0,j-1
          temp = n(r)/(right(r+1)+left(j-r))
          n(r) = saved + right(r+1)*temp
          saved = left(j-r)*temp
       end do
       n(j) = saved
    end do
  end subroutine basisfuncs

!============================================================================================================
!============================================================================================================
  
!  differentiation of bsp in forward (tangent) mode:
!  variations  of output variables: xnode ynode
!  with respect to input variables: x y
  subroutine bsp_d(x, xd, y, yd, p, control, u, u_point, xnoded, ynoded)
    implicit none
    integer :: p
    integer :: control
    real(wp), dimension(0:) :: u
    real(wp), dimension(0:p) :: n
    real(wp) :: u_point
    integer :: i, span
    real(wp) :: xnoded, ynoded
    real(wp), dimension(0:) :: x, y
    real(wp), dimension(0:) :: xd, yd
    
    span = findspan(u_point, p, control, u)
    call basisfuncs(span, u_point, p, u, n)
    xnoded = 0.0
    ynoded = 0.0
    do i=0,p
       xnoded = xnoded + n(i)*xd(span-p+i)
       ynoded = ynoded + n(i)*yd(span-p+i)
    end do
  end subroutine bsp_d
  
!============================================================================================================
!============================================================================================================
 
  subroutine bsp_line(x, y, z, p, control, u, u_point, xnode, ynode, znode)
    implicit none
    integer :: p
    integer :: control
    real(wp), dimension(0:) :: u
    real(wp), dimension(0:p) :: n
    real(wp) :: u_point
    integer :: i, span
    real(wp) :: xnode, ynode, znode
    real(wp), dimension(0:) :: x, y, z
    
    span = findspan(u_point, p, control, u)
    call basisfuncs(span, u_point, p, u, n)
    xnode = 0.
    ynode = 0.
    znode = 0.
    do i=0,p
       xnode = xnode + n(i)*x(span-p+i)
       ynode = ynode + n(i)*y(span-p+i)
       znode = znode + n(i)*z(span-p+i)
    end do
  end subroutine bsp_line

  
!============================================================================================================
!============================================================================================================


  subroutine dersbasisfuncs(i, u_p, p, n, u, ders)
    implicit none
    integer :: i, p, k, n, r, s1, s2, j, j1, j2, pk, rk
    real(wp) :: u_p, saved, temp, d
    real(wp), dimension (0:p+1,0:p+1) :: ndu
    real(wp), dimension (0:2,0:p+1) :: a 
    real(wp), dimension(p) :: left, right
    real(wp), dimension (0:,0:) :: ders
    real(wp), dimension (0:) :: u

    ndu(0,0) = 1.0
    do j=1, p 
       left(j) = u_p - u(i+1-j)
       right(j) = u(i+j) - u_p
       saved = 0.
       do r=0, j-1
          ndu(j,r) = right(r+1) + left(j-r)
          temp = ndu(r,j-1) / ndu(j,r)
          ndu(r,j) = saved + right(r+1) * temp
          saved = left(j-r) * temp
       enddo
       ndu(j,j) = saved
    enddo
    do j=0, p
       ders(0,j) = ndu(j,p)
    enddo
    do r=0, p
       s1 = 0
       s2 = 1
       a(0,0) = 1.0
       do k=1, n
          d = 0.0
          rk = r - k
          pk = p - k
          if(r >= k) then
             a(s2,0) = a(s1,0) / ndu(pk+1,rk)
             d = a(s2,0) * ndu(rk,pk)
          endif
          if(rk >= -1) then
             j1 = 1
          else
             j1 = -rk
          endif
          if(r-1 <= pk) then 
             j2 = k - 1
          else
             j2 = p - r
          endif
          do j=j1, j2
             a(s2,j) = (a(s1,j) - a(s1,j-1)) / ndu(pk+1,rk+j)
             d = d + a(s2,j) * ndu(rk+j,pk)
          enddo
          if(r <= pk) then
             a(s2,k) = -a(s1,k-1) / ndu(pk+1,r)
             d = d + a(s2,k) * ndu(r,pk)
          endif
          ders(k,r) = d
          j = s1
          s1 = s2
          s2 = j
       enddo
    enddo
    r = p
    do k=1, n
       do j=0, p
          ders(k,j) = ders(k,j) * r
       enddo
       r = r * (p - k)
    enddo
  end subroutine dersbasisfuncs
  

!============================================================================================================
!============================================================================================================


  subroutine curveDerivsAlg1(x, y, z, p, control, u, u_0, d, ckx, cky, ckz)
    implicit none
    integer :: p
    integer :: control
    integer :: k, d, j, span, du
    real(wp) :: u_0
    real(wp), dimension (0:) :: u
    real(wp), dimension(0:) :: x, y, z
    real(wp), dimension (0:p) :: ckx, cky, ckz
    real(wp), dimension (:,:), allocatable :: nders
    
    du = min(d, p)
    allocate (nders(0:du, 0:p))
    do k=p+1, d
       ckx(k) = 0.
       cky(k) = 0.
       ckz(k) = 0.
    enddo
    
    span = findspan(u_0, p, control, u)
    call dersbasisfuncs(span, u_0, p, du, u, nders)
    
    do k=0, du
       ckx(k) = 0.
       cky(k) = 0.
       ckz(k) = 0.
       do j=0, p
          ckx(k) = ckx(k) + nders(k,j) * x(span-p+j)
          cky(k) = cky(k) + nders(k,j) * y(span-p+j)
          ckz(k) = ckz(k) + nders(k,j) * z(span-p+j)
       enddo
    enddo
  end subroutine curveDerivsAlg1


!============================================================================================================
!============================================================================================================


  subroutine surfaceDerivsAlg1(ctl_u, p_u, U, ctl_v, p_v, V, Ctl_p, u_point, v_point, d, SKL) 
    implicit none
    integer :: p_u, p_v
    integer :: ctl_u, ctl_v
    real(wp), dimension(0:) :: U, V
    real(wp) :: u_point, v_point
    integer :: r, s, k, l, uspan, vspan, d, dd, du, dv
    real(wp), dimension (0:p_u,0:p_u) :: Nu
    real(wp), dimension (0:p_v,0:p_v) :: Nv
    type(point_t), dimension(0:, 0:) :: Ctl_p
    type(point_t), dimension (0:p_v) :: temp
    type(point_t), dimension (0:p_u, 0:p_v) :: SKL


    du = min(d, p_u)
    do k = p_u + 1, d
       do l = 0, d - k
          SKL(k,l)%x= 0.
       enddo
    enddo

    dv = min(d, p_v)
    do l = p_v + 1, d
       do k = 0, d - l
          SKL(k,l)%x = 0.
       enddo
    enddo

    uspan = findspan(u_point, p_u, ctl_u, U)
    call dersbasisfuncs(uspan, u_point, p_u, du, U, Nu)
    vspan = findspan(v_point, p_v, ctl_v, V)
    call dersbasisfuncs(vspan, v_point, p_v, dv, V, Nv)

    DO k = 0, du
       do s = 0, p_v
          temp(s)%x = 0.
          do r = 0, p_u
             temp(s)%x = temp(s)%x + Nu(k,r) * Ctl_p(uspan-p_u+r, vspan-p_v+s)%x
          enddo
       enddo

       dd = min(d-k, dv)
       do l = 0, dd
          SKL(k,l)%x = 0.
          do s = 0, p_v
             SKL(k,l)%x = SKL(k,l)%x + Nv(l,s) * temp(s)%x
          enddo
       enddo
    ENDDO
  end subroutine surfaceDerivsAlg1


!  Differentiation of surfacederivsalg1 in forward (tangent) mode:
!   variations   of useful results: skl.x
!   with respect to varying inputs: skl.x ctl_p.x
!============================================================================================================
!============================================================================================================


  SUBROUTINE SURFACEDERIVSALG1_D(ctl_u, p_u, u, ctl_v, p_v, v, ctl_p, &
       &                         ctl_pd, u_point, v_point, d, skl, skld)
    IMPLICIT NONE
    INTEGER :: p_u, p_v
    INTEGER :: ctl_u, ctl_v
    REAL(wp), DIMENSION(0:) :: u, v
    REAL(wp) :: u_point, v_point
    INTEGER :: r, s, k, l, uspan, vspan, d, dd, du, dv
    REAL(wp), DIMENSION(0:p_u, 0:p_u) :: nu
    REAL(wp), DIMENSION(0:p_v, 0:p_v) :: nv
    TYPE(POINT_T), DIMENSION(0:, 0:) :: ctl_p
    TYPE(POINT_T), DIMENSION(0:, 0:) :: ctl_pd
    TYPE(POINT_T), DIMENSION(0:p_v) :: temp
    TYPE(POINT_T), DIMENSION(0:p_v) :: tempd
    TYPE(POINT_T), DIMENSION(0:p_u, 0:p_v) :: skl
    TYPE(POINT_T), DIMENSION(0:p_u, 0:p_v) :: skld
    INTRINSIC MIN
    INTEGER :: ii1

    IF (d .GT. p_u) THEN
       du = p_u
    ELSE
       du = d
    END IF
    DO k=p_u+1,d
       DO l=0,d-k
          skld(k, l)%x = 0.0
          skl(k, l)%x = 0.
       END DO
    END DO
    IF (d .GT. p_v) THEN
       dv = p_v
    ELSE
       dv = d
    END IF
    DO l=p_v+1,d
       DO k=0,d-l
          skld(k, l)%x = 0.0
          skl(k, l)%x = 0.
       END DO
    END DO
    uspan = FINDSPAN(u_point, p_u, ctl_u, u)
    CALL DERSBASISFUNCS(uspan, u_point, p_u, du, u, nu)
    vspan = FINDSPAN(v_point, p_v, ctl_v, v)
    CALL DERSBASISFUNCS(vspan, v_point, p_v, dv, v, nv)
    DO ii1=0,p_v
       tempd(ii1)%x = 0.0
    END DO
    DO k=0,du
       DO s=0,p_v
          tempd(s)%x = 0.0
          temp(s)%x = 0.
          DO r=0,p_u
             tempd(s)%x = tempd(s)%x + nu(k,r) * ctl_pd(uspan - p_u + r,vspan - p_v + s)%x
             temp(s)%x = temp(s)%x + nu(k,r) * ctl_p(uspan - p_u + r,vspan - p_v + s)%x
          END DO
       END DO
       IF (d - k .GT. dv) THEN
          dd = dv
       ELSE
          dd = d - k
       END IF
       DO l=0,dd
          skld(k, l)%x = 0.0
          skl(k, l)%x = 0.
          DO s=0,p_v
             skld(k, l)%x = skld(k, l)%x + nv(l, s)*tempd(s)%x
             skl(k, l)%x = skl(k, l)%x + nv(l, s)*temp(s)%x
          END DO
       END DO
    END DO
  END SUBROUTINE SURFACEDERIVSALG1_D


!============================================================================================================
!============================================================================================================


  subroutine surfaceDerivsAlg1_matrix(ctl_u, p_u, U, ctl_v, p_v, V, u_point, v_point, uspan, vspan, Rij) 
    implicit none
    integer :: i, j, p_u, p_v
    integer :: ctl_u, ctl_v, counter_u, counter_v
    real(wp), dimension(0:) :: U, V
    real(wp) :: u_point, v_point
    integer :: uspan, vspan
    real(wp), dimension (0:p_u) :: Nu
    real(wp), dimension (0:p_v) :: Nv
    real(wp), dimension(0:ctl_u,0:ctl_v) :: Rij


    uspan = findspan(u_point, p_u, ctl_u, U)
    call basisfuncs(uspan, u_point, p_u, U, Nu)
    vspan = findspan(v_point, p_v, ctl_v, V)
    call basisfuncs(vspan, v_point, p_v, V, Nv)

    Rij = 0.0d0

    counter_u = -1
    do i=0,ctl_u
       counter_v = -1
       if(i.ge.uspan-p_u .and. i.le.uspan) then
          counter_u = counter_u + 1
          do j=0,ctl_v
             if(j.ge.vspan-p_v .and. j.le.vspan) then
                counter_v = counter_v + 1
                Rij(i,j) = Nu(counter_u) * Nv(counter_v)
             end if
          end do
       end if
    end do



    ! DO k = 0, du
    !    do s = 0, p_v
    !       temp(s)%x = 0.
    !       do r = 0, p_u
    !          temp(s)%x = temp(s)%x + Nu(k,r) * Ctl_p(uspan-p_u+r, vspan-p_v+s)%x
    !       enddo
    !    enddo

    !    dd = min(d-k, dv)
    !    do l = 0, dd
    !       SKL(k,l)%x = 0.
    !       do s = 0, p_v
    !          SKL(k,l)%x = SKL(k,l)%x + Nv(l,s) * temp(s)%x
    !       enddo
    !    enddo
    ! ENDDO
  end subroutine surfaceDerivsAlg1_matrix

!============================================================================================================
!============================================================================================================


end module nurbs_m
