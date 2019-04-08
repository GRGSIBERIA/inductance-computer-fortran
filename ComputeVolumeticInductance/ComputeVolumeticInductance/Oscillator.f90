    module OscillatorClass
    implicit none
    
    ! 振動体クラス
    type Oscillator
        character*32 partName
        double precision, dimension(:,:,:), allocatable :: positions     ! パート単位でレポートの座標値
    end type
    
    contains
    
    ! numofNodes は maximumNodeIdと同じ数を宣言している
    type(Oscillator) function init_Oscillator(part, reports, numofTimes, numofNodes) result(this)
        USE ReportFileClass
        implicit none
        character*32, intent(in) :: part
        type(ReportFile), dimension(:), intent(in) :: reports
        integer, intent(in) :: numofTimes, numofNodes
        integer i
        
        this%partName = part
        ALLOCATE (this%positions(3,numofTimes, numofNodes))
        this%positions = 0
        
        do i = 1, SIZE(reports)
            if (part == reports(i)%partName) then
                this%positions = this%positions + reports(i)%positions
            end if
        end do
        
        
    end function
    
    end module