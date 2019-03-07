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
    type(Coil) function init_Coil(height, radius, numofTimes) result(this)
        real, intent(in) :: height, radius
        integer, intent(in) :: numofTimes
        
        this%height = height
        this%radius = radius
        
        ALLOCATE (this%centroid(numofTimes, 3))
        ALLOCATE (this%forward(numofTimes, 3))
        ALLOCATE (this%right(numofTimes, 3))
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