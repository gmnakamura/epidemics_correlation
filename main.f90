program simulation
  use subroutines
  use statistics
  implicit real*8 (a-h,o-z)

  integer,parameter::inum_stats = 4 
  integer,parameter::inum_corrs = 4
  real         ::rng
  real*8       ::params(2)
  character*128::ifilename
  
  integer*1,allocatable::iconfig(:)
  real*8   ,allocatable::datum(:,:)
  real*8   ,allocatable::data(:,:)
  real*8   ,allocatable::corr(:,:)

  !call cpu_time(current_time)
  current_time =omp_get_wtime() ! openmp timing

  call read_args(N,isteps,isamples,params,ifilename)
  
  allocate(iconfig(0:N-1))
  allocate(datum(isteps,inum_stats))
  allocate( data(isteps,inum_stats+inum_corrs))  
  data = 0d0
  allocate(    corr(isteps,inum_corrs))
  corr     = 0d0

  !$OMP PARALLEL DO PRIVATE(iconfig,datum,k,rng,corr_tmp)
  do isample = 1,isamples
     call initialize(iconfig)
     do k = 1, isteps
        ! seeds are private for each thread
        call random_number(rng) ! consider mersenne twister from mkl
        call update(N,iconfig,params,rng)
        call density(N,iconfig,datum(k,:))
     end do
     call correlation(N,datum(:,1),corr) ! density density corr
     !$OMP CRITICAL
     call update_mc_density(N,isamples,datum,data)
     !$OMP END CRITICAL
     !$OMP CRITICAL     
     call update_mc_correlation(N,isamples,inum_stats + 1,corr,data)
     !$OMP END CRITICAL
     
     !call update_mc_correlation(N,isamples,corr_tmp,corr)
     
  end do
  !$OMP END PARALLEL DO

  !call cpu_time(elapsed_time)
  elapsed_time = omp_get_wtime() ! openmp timing
  
  elapsed_time = elapsed_time - current_time
  print *,'elapsed time :: ',elapsed_time
  1010 format (I8,2I12,ES20.12E2,ES20.12E2,ES20.12E2)
  open(9,file='log_performance.dat',access='append',status='unknown')
  write(9,1010) int(N),int(isteps),int(isamples),params(1)*N,params(2)*N*N,elapsed_time
  close(9)

  call save(trim(ifilename),data)
  
end program simulation
