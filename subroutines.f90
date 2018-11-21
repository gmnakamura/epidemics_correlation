module subroutines
  implicit real*8 (a-h,o-z)
  complex*16:: cima = (0d0,1d0)
  
contains

  subroutine update(N,iconfig,params,rng)
    implicit real*8 (a-h,o-z)
    integer*1::iconfig(0:N-1)
    real*8   ::params(2)
    real     ::rng
    
    current_probability = 0d0
    do k = 0 , N-1
       current_probability = current_probability+iconfig(k)*params(1)
       if (rng <  current_probability) then
          iconfig(k) = 0
          return
       end if
    end do

    do k = 0,N-1
       local_iconfig = iconfig(k)
       !
       ! complete graph
       !
       do j = 0,N-1          
          current_probability = current_probability + local_iconfig*(1-iconfig(j))*params(2)
          if (rng < current_probability) then
             iconfig(j) = 1
             return
          end if
       end do
    end do    
  end subroutine update

!!$--------------------------------------------------

  subroutine initialize(iconfig)
    integer*1,intent(out)::iconfig(:)
    iconfig = 0
    do i = 1,size(iconfig),4
       iconfig(i) = 1
    end do
  end subroutine initialize

!!$--------------------------------------------------

  subroutine read_args(N,isteps,isamples, params,ifilename)
    character(len=32)::arg
    real*8 ::fargs(5)
    real*8 ,intent(out)::params(2)
    integer,intent(out)::N,isteps,isamples
    character(len=*),intent(out)::ifilename
    ! default values
    ! N = 10 , steps = 100 , samples =100 , gamma = 0.3, alpha = 1.0
    fargs = (/ 1d1, 1d2 , 1d2 , 0.3d0 , 1d0 /)

    ifilename =  "data"    
    
    ntotal = command_argument_count() 
    do i = 1,ntotal,2
       call get_command_argument(i,arg)
       select case(trim(arg))
       case ('-n','-N')
          call get_command_argument(i+1,arg)
          read(arg,*) fargs(1)
          ifilename = trim(ifilename)//'_N'//trim(arg)
       case ('--steps','-s')
          call get_command_argument(i+1,arg)
          read(arg,*) fargs(2)
          ifilename = trim(ifilename)//'_steps'//trim(arg)
       case ('--samples','-mc')
          call get_command_argument(i+1,arg)
          read(arg,*) fargs(3)
          ifilename = trim(ifilename)//'_samples'//trim(arg)
       case ('--gamma','-g')          
          call get_command_argument(i+1,arg)
          read(arg,*) fargs(4)
          ifilename = trim(ifilename)//'_gamma'//trim(arg)
       case ('--alpha','-a')          
          call get_command_argument(i+1,arg)
          read(arg,*) fargs(5)
          ifilename = trim(ifilename)//'_alpha'//trim(arg)
       case('-h','--help')
          print *,'example usage'
          print *,'      main -n 10 --samples 100 --steps 100 --alpha 1.0 --gamma 0.3'
          call exit(0)
       end select
    end do
    ifilename = trim(ifilename)//'.dat'
    print *,ifilename

    N = int(fargs(1))
    isteps   = int(fargs(2))
    isamples = int(fargs(3))
    params(1)= fargs(4)/N
    params(2)= fargs(5)/(N*N)
  end subroutine read_args

!!$--------------------------------------------------
  
  subroutine save(ifilename,data)
    implicit real*8 (a-h,o-z)   
    real*8::data(:,:)
    character(len=128)::ifilename
    character(len=32) ::fmts
    
    m = size(data,2)
    write(fmts,"(I0)") m+1
    fmts ="(I8,A,"//trim(fmts)//"ES20.12E2)"

    
    open(10,file=trim(ifilename))
    do k =1,size(data,1)
       write(10,fmts) k,' ',(data(k,j),j=1,m)
    end do
    close(10)
  end subroutine save
  
  
end module subroutines
