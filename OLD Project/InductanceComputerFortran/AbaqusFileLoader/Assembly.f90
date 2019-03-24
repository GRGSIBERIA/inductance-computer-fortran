    module AssemblyClass
    implicit none
    
    ! 節点情報を持つ構造型
    type Assembly
        integer numofNodes
        integer, dimension(:), allocatable :: nodeIds
        real, dimension(:,:), allocatable :: positions
    end type
    
    ! コンストラクタ宣言
    interface Assembly
        module procedure init_Assembly
    end interface
    
    contains
    
    ! コンストラクタ
    type(Assembly) function init_Assembly(numofNodes) result(this)
        integer, intent(in) :: numofNodes
        
        this%numofNodes = numofNodes
        ALLOCATE (this%positions(numofNodes, 3))
        ALLOCATE (this%nodeIds(numofNodes))
    end function
    
    end module