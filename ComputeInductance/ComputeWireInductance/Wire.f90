    module WireClass
    use InputFileClass
    use ReportFileClass
    implicit none
    
    type Wire
        type(InputFile) input
        type(ReportFile) assembly
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
    end function
    
    end module