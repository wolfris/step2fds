module type_dec
use constants_m

real(wp),parameter,private::inf=1.d+20

type point_t
  real(wp), dimension (3) :: x
end type point_t

type cpts_t
  real(wp),dimension(3)::x=0.0
  real(wp) :: color=0
  logical :: joint = .false.
  logical :: border = .false.
  integer :: bsp_on, step_id, n_mvbldir, n_dirdep
  integer, dimension(:), allocatable :: mvbldir, dirdep
  real(wp), dimension (:), allocatable :: curvature
  type(point_t), dimension (:), allocatable :: norm
end type cpts_t

type bsp_t
  type (cpts_t),dimension(:), allocatable :: alpha
  integer, dimension (:), allocatable :: cpts_id
  real(wp),dimension(:), allocatable :: knot
  integer :: p
  integer :: mctl
  integer :: mknot
  integer :: nfmv    !ID of first moveable control point
  integer :: step_id   !Entry number of the spline in the step file
  type(point_t), dimension (:), allocatable :: surf_pts, surf_grad, surf_sec_dev
end type bsp_t




end module type_dec
