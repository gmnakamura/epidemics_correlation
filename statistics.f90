module statistics


  
contains
  !!$--------------------------------------------------

  subroutine density(N,iconfig,data)
    implicit real*8 (a-h,o-z)
    integer  ::N
    real*8   ::data(:)
    integer*1::iconfig(:)
    data( 1 ) = sum(iconfig)*1d0/N
    data( 2 ) = sum((iconfig - data(1))**2)/(N-1)
    return
  end subroutine density

!!$--------------------------------------------------

  subroutine update_mc_density(N,isamples,datum,data)
    implicit real*8 (a-h,o-z)
    real*8   ::data(:,:),datum(:,:)
    isteps = size(datum,1)
    istats = size(datum,2)
    r      = 1d0/isamples    
    do i = 1,4
       data(:,i ) = data(:,i )+r*(datum(:,1)**i)       
    end do
    return
  end subroutine update_mc_density
  
!!$--------------------------------------------------
  
  subroutine correlation(N,datum,corr)
    implicit real*8(a-h,o-z)
    integer::N
    real*8,intent(in ) ::datum(:)
    real*8,intent(out) ::corr(:,:)
    imax_delta = size(corr,2)/2
    isteps = size(datum,1)
    do k =1, imax_delta
       do istep = 1, (isteps - imax_delta)
          corr(istep,2*k-1) = datum(istep)* datum(istep+k)
          corr(istep,2*k  ) = datum(istep)*(datum(istep+k)**2)
       end do
    end do
    return
  end subroutine correlation
  
!!$--------------------------------------------------
  
  subroutine update_mc_correlation(N,isamples,ishift,corr1,corr2)
    implicit real*8(a-h,o-z)
    integer::N
    integer::isamples
    integer::ishift
    real*8,intent(in)   ::corr1(:,:)
    real*8,intent(inout)::corr2(:,:)
    r      = 1d0/real(isamples,8)
    isize  = size(corr2,2)
    corr2(:,ishift:isize) = corr2(:,ishift:isize) + ( corr1 * r )
    return 
  end subroutine update_mc_correlation
  
end module statistics
