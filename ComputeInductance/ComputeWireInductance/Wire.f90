    module WireClass
    use InputFileClass
    use ReportFileClass
    implicit none
    
    type Wire
        type(InputFile) input
        type(ReportFile) assembly
        real, dimension(:,:), allocatable :: fluxes
        integer :: numofTimes, numofNodes
    end type
    
    interface Wire
        module procedure :: init_Wire
    end interface
    
    contains
    
    type(Wire) function init_Wire(conf, wireCount) result(this)
        use ConfigClass
        implicit none
        type(Config), intent(in) :: conf
        integer, intent(in) :: wireCount
        
        this%input = init_InputFile(conf%inputFD, conf%wirePartNames(wireCount))
        this%assembly = init_ReportFile(conf%wireFDs(wireCount), this%input)
        this%numofTimes = SIZE(this%assembly%times)
        this%numofNodes = SIZE(this%assembly%nodeIds)
        
        ALLOCATE (this%fluxes(this%numofTimes, this%numofNodes))
    end function
    
    end module