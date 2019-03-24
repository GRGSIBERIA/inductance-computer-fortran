    module CoilClass
    
    implicit none
    
    type Coil
        real :: radius
        real :: height
        real, dimension(:,:), allocatable :: centroid    ! 中心座標(時間,3)
        real, dimension(:,:), allocatable :: forward     ! 前向きベクトル(時間,3)
        real, dimension(:,:), allocatable :: right       ! 右向きベクトル(時間,3)
    end type
    
    interface Coil
        module procedure :: init_Coil
    end interface
    
    contains
    
    ! コイルのコンストラクタ
    type(Coil) function init_Coil(height, radius, frontId, backId, coilXY) result(this)
        use XYDataClass
        implicit none
        real, intent(in) :: height, radius
        integer, intent(in) :: frontId, backId
        type(XYData), intent(in) :: coilXY
        
        integer numofTimes, timeid, nodeid
        integer front, right
        real, dimension(3) :: center
        
        numofTimes = SIZE(coilXY%times)
        this%height = height
        this%radius = radius
        
        ALLOCATE (this%centroid(numofTimes, 3))
        ALLOCATE (this%forward(numofTimes, 3))
        ALLOCATE (this%right(numofTimes, 3))
        
        ! 配列を分けているのはベクトル最適化をかけやすくしたい
        ! 中心座標の入力
        DO timeid = 1, numofTimes
            center = 0
            DO nodeid = 1, coilXY%numofNodes
                center(:) = center(:) + coilXY%displaces(timeid, nodeid, :)
            END DO
            this%centroid(timeid,:) = center
        END DO
        
        ! 正面データの入力
        DO timeid = 1, numofTimes
            DO nodeid = 1, coilXY%numofNodes
                
            END DO
        END DO
        
        ! 右手データの入力
        DO timeid = 1, numofTimes
            DO nodeid = 1, coilXY%numofNodes
                
            END DO
        END DO
    end function
    
    ! FluxDensity系の関数に渡せるような形に直す関数
    subroutine SummarizeCoilSettings(timeid, numofCoils, coils, coilPositions, coilForwards, coilRights, coilHeights, coilRadius)
        implicit none
        integer, intent(in) :: numofCoils, timeid
        type(Coil), dimension(numofCoils), intent(in) :: coils
        real, dimension(numofCoils, 3), intent(out) :: coilForwards, coilRights, coilPositions
        real, dimension(numofCoils), intent(out) :: coilHeights, coilRadius
        
        integer i
        
        DO i = 1, numofCoils
            coilPositions(i,:) = coils(i)%centroid(timeid,:)
            coilForwards(i,:) = coils(i)%forward(timeid,:)
            coilRights(i,:) = coils(i)%right(timeid,:)
            coilHeights(i) = coils(i)%height
            coilRadius(i) = coils(i)%radius
        END DO
        
    end subroutine
    
    end module