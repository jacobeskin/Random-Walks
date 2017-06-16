program twoDwalk

  !Random walk on a 2D continuum plane (i.e. not a grid) with 2 possible step
  !lengths l_1 and l_2. 3 walkers with 1000 steps. Walkers direction on
  !every step is determined by a randomly generated angle. Outputs are a text
  !files of the paths for plotting .

  use mtdefs
  use mtmod

  implicit none

  integer, parameter :: rkk = selected_real_kind(10,40)
  integer, parameter :: N_steps = 1000
  real(kind=rkk) :: x1, y1, x2, y2, x3, y3, pi
  real(kind=rkk) :: u1, u2, u3, phi1, phi2, phi3, v
  integer :: l_1, l_2, l, i, j, iarg, n1, n2
  character(len=80) :: arg
  
  ! Defining pi
  pi = 4*atan(1.0_rkk)

  ! Initializing the random number generator
  call sgrnd(getseed(info=1))

  ! Reading in values for l_1 and l_2 from command line
  iarg = command_argument_count()
  if (iarg/=2) then
     print'("Two arguments, l_1 and l_2!!")'
  end if
  call get_command_argument(1,arg)
  read(arg,*) l_1
  call get_command_argument(2,arg)
  read(arg,*) l_2

  ! Set walk
  x1 = 0
  y1 = 0
  x2 = 0
  y2 = 0
  x3 = 0
  y3 = 0

  open(unit=1, file='walker1', status='unknown')
  open(unit=2, file='walker2', status='unknown')
  open(unit=3, file='walker3', status='unknown')
  do i=1,N_steps
     
     u1 = grnd() ! Random number for the angle1
     u2 = grnd() ! for angle2
     u3 = grnd() ! for angle3
     
     phi1 = 2*pi*u1 ! The direction of the next step for 1
     phi2 = 2*pi*u2 ! for 2
     phi3 = 2*pi*u3 ! for 3
     
     v = igrnd(0,1) ! Random number to decide between l_1 and l_2 for 1
     if (v==0) then
        l = l_1
        n1 = n1+1
     else
        l = l_2
        n2 = n2+1
     end if
     ! Taking the step
     x1 = x1+l*cos(phi1)
     y1 = y1+l*sin(phi1)
     write(1,*) x1, y1

     v = igrnd(0,1) ! Random number to decide between l_1 and l_2 for 2
     if (v==0) then
        l = l_1
        n1 = n1+1
     else
        l = l_2
        n2 = n2+1
     end if
     ! Taking the step
     x2 = x2+l*cos(phi2)
     y2 = y2+l*sin(phi2)
     write(2,*) x2, y2

     v = igrnd(0,1) ! Random number to decide between l_1 and l_2 for 3
     if (v==0) then
        l = l_1
        n1 = n1+1
     else
        l = l_2
        n2 = n2+1
     end if
     ! Taking the step
     x3 = x3+l*cos(phi3)
     y3 = y3+l*sin(phi2)
     write(3,*) x3, y3
 
  end do
  close(1)
  close(2)
  close(3)
  
end program twoDwalk

        
