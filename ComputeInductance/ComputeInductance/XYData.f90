    module XYDataClass
    implicit none
    
    ! XYデータの集合
    type XYData
        real, dimension(:), allocatable :: times                ! 時間
        integer, dimension(:), allocatable :: nodeIds           ! ノード番号
        real, dimension(:,:,:), allocatable :: displaces        ! 3次元の位置
        real, dimension(:,:,:), allocatable :: unsortDisplaces  ! 未整理の位置
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
        ALLOCATE (this%nodeIds(numofData))
        ALLOCATE (this%displaces(numofTimes, numofData / 3, 3)) ! numofDataはデータの総数なので，XYZで分けると3で割らないといけない
        ALLOCATE (this%unsortDisplaces(numofTimes, numofData, 3))
        
        this%nodeIds = 0
        this%unsortDisplaces = 0
        this%displaces = 0
        
    end function
    
    end module