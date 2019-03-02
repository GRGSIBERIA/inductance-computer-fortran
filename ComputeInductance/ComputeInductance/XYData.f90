    module XYDataClass
    implicit none
    
    ! XYデータの集合
    type XYData
        real, dimension(:), allocatable :: times
        real, dimension(:,:,:), allocatable :: displace ! 3次元で固定
    end type
    
    ! コンストラクタ宣言
    interface XYData
        module procedure init_XYData
    end interface
    
    contains
    
    ! コンストラクタ
    type(XYData) function init_XYData(numofTimes, numofData) result(this)
        integer, intent(in) :: numofTimes, numofData
        
        ALLOCATE (this%times(numofTimes))
        ALLOCATE (this%displace(numofTimes, numofData, 3))
    end function
    
    end module