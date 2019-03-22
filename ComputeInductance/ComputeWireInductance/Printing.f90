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
    
    subroutine WriteCoilInductance(outputFD, times, inductances)
        implicit none
        integer, intent(in) :: outputFD
        real, dimension(:), intent(in) :: times, inductances
        integer ti
        
        WRITE (outputFD, *) "*Coil", ",", SIZE(inductances)
        do ti = 1, SIZE(inductances)
            WRITE (outputFD, *) times(ti), ",", inductances(ti)
        end do
        
    end subroutine
    
    end module
    