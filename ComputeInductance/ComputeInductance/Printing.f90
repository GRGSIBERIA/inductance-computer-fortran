    module PrintingMod
    implicit none
    
    contains
    
    !subroutine WriteCoilsInductance(outFD, coils)
    !    use CoilClass
    !    implicit none
    !    integer, intent(in) :: outFD
    !    type(Coil), dimension(:), intent(in) :: coils
    !    integer ci, ti
    !    
    !    do ci = 1, SIZE(coils)
    !        WRITE (outFD, *) "*Coil", ci
    !        do ti = 1, coils(ci)%numofTimes
    !            WRITE (outFD, *) coils(ci)%top%times(ti), coils(ci)%inductances(ti)
    !        end do
    !    end do
    !    
    !end subroutine
    
    subroutine WriteCoilInductance(outputFD, times, fluxes, inductances)
        implicit none
        integer, intent(in) :: outputFD
        double precision, dimension(:), intent(in) :: times, inductances, fluxes
        integer ti
        
        WRITE (outputFD, *) "*Coil", ",", SIZE(inductances)
        do ti = 1, SIZE(times)
            WRITE (outputFD, *) times(ti), ",", fluxes(ti), ",", inductances(ti)
        end do
        
    end subroutine
    
    end module
    