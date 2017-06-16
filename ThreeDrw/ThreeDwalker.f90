program ThreeDwalker
  !Compilation: $ make -f make3Dwalker
  !Running: $ ./ThreeDwalker.exe

  !Random walk on a 3D grid with step length 3. Uses 3 walkers with 1000
  !steps. The code prints out the path to a file for plotting.


  ! Used modules
  use mtdefs
  use mtmod

  implicit none

  integer, parameter :: N_steps = 1000
  integer :: i, j, k
  integer :: u1, u2, u3, v1, v2, v3, l
  integer :: x1, y1, z1, x2, y2, z2, x3, y3, z3

  ! Initializing the random number generator
  call sgrnd(getseed(info=1))

  ! Reset the walk
  x1 = 0
  y1 = 0
  z1 = 0
  x2 = 0
  y2 = 0
  z2 = 0
  x3 = 0
  y3 = 0
  z3 = 0
  
  ! Starting the runs (1000 steps in total).
  open(unit=1, file='walker1', status='unknown')
  open(unit=2, file='walker2', status='unknown')
  open(unit=3, file='walker3', status='unknown')
  do i=1,N_steps
     ! Loop for a random walk
     u1 = igrnd(1,3) ! Random integer for step direction: x=1, y=2, z=3
     v1 = igrnd(0,1) ! Random integer to detrmine sign for l: 0=-, 1=+
     u2 = igrnd(1,3) 
     v2 = igrnd(0,1)
     u3 = igrnd(1,3) 
     v3 = igrnd(0,1)
     
     ! Walker 1
     if (v1==0) then
        l = -1
     else if (v1==1) then
        l = +1
     end if
     if (u1==1) then
        x1 = x1+l         ! Move in x direction
     else if (u1==2) then
        y1 = y1+l         ! Move in y direction
     else if (u1==3) then
        z1 = z1+l         ! Move in z direction
     end if
     
     write(1,*) x1, y1, z1
     
     ! Walker 2
     if (v2==0) then
        l = -1
     else if (v2==1) then
        l = +1
     end if
     if (u2==1) then
        x2 = x2+l         ! Move in x direction
     else if (u2==2) then
        y2 = y2+l         ! Move in y direction
     else if (u2==3) then
        z2 = z2+l         ! Move in z direction
     end if
     write(2,*) x2, y2, z2
     
     ! Walker 3
     if (v3==0) then
        l = -1
     else if (v3==1) then
        l = +1
     end if
     if (u3==1) then
        x3 = x3+l         ! Move in x direction
     else if (u3==2) then
        y3 = y3+l         ! Move in y direction
     else if (u2==3) then
        z3 = z3+l         ! Move in z direction
     end if
     write(3,*) x3, y3, z3
     
  end do
    
  close(1)
  close(2)
  close(3)

end program ThreeDwalker

  

  
        
