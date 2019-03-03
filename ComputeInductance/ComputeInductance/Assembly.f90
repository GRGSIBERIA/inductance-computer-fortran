    module AssemblyClass
    implicit none
    
    ! 節点情報を持つ構造型
    type Assembly
        integer numofNodes
        integer, dimension(:), allocatable :: nodeIds
        real, dimension(:,:), allocatable :: positions
        
    contains
        procedure :: AddDisplacement => Assembly_AddDisplacement
        procedure :: ReturnPosition => Assembly_ReturnPosition
    end type
    
    ! コンストラクタ宣言
    interface Assembly
        module procedure init_Assembly
    end interface
    
    contains
    
    ! コンストラクタ
    type(Assembly) function init_Assembly(numofNodes) result(this)
        implicit none
        integer, intent(in) :: numofNodes
        
        this%numofNodes = numofNodes
        ALLOCATE (this%positions(numofNodes, 3))
        ALLOCATE (this%nodeIds(numofNodes))
    end function
    
    
    subroutine Assembly_AddDisplacement(this, nodeId, vector)
        implicit none
        class(Assembly) this
        integer, intent(in) :: nodeId
        real, dimension(3) :: vector
    
        integer i
        
        ! ノード番号を見つけたら脱出
        DO i = 1, this%numofNodes
            IF (nodeId == this%nodeIds(i)) THEN
                GOTO 4000
            END IF
        END DO
4000    continue
        
        this%positions(i,:) = this%positions(i,:) + vector
    end subroutine
    
    
    function Assembly_ReturnPosition(this, nodeId) result(position)
        implicit none
        class(Assembly) this
        integer, intent(in) :: nodeId
        real, dimension(3) :: position
        
        integer i
        
        DO i = 1, this%numofNodes
            IF (nodeId == this%nodeIds(i)) THEN
                GOTO 4100
            END IF
        END DO
4100    continue
        
        position = this%positions(i, :)
    end function
    
    end module