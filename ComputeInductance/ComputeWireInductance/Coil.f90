    module CoilClass
    use ReportFileClass
    use InputFileClass
    implicit none
    
    type Coil
        real :: radius, height
        type(InputFile) :: input
        type(ReportFile) :: top, bottom
        real, dimension(:,:), allocatable :: forward, right
    end type
    
    interface Coil
        module procedure :: init_Coil
    end interface
    
    contains
    
    ! 中心座標を計算する関数
    function Coil_CenterPosition(size, positions) result(center)
        implicit none
        integer, intent(in) :: size
        real, dimension(size,3), intent(in) :: positions
        real, dimension(3) :: center
        integer i
        
        center = 0
        do i = 1, size
            center = center + positions(i,:)
        end do
        center = center / real(size)
    end function
    
    ! 高さを計算する関数
    real function Coil_Height(top, bottom) result(h)
        implicit none
        real, dimension(3), intent(in) :: top, bottom
        real, dimension(3) :: temp
        
        temp = top - bottom
        h = SQRT(DOT_PRODUCT(temp, temp))
    end function
    
    ! 半径を計算する関数
    real function Coil_Radius(size, positions, center) result(r)
        implicit none
        integer, intent(in) :: size
        real, dimension(size,3), intent(in) :: positions
        real, dimension(3), intent(in) :: center
        real, dimension(3) :: dot
        real max, tmp
        integer i
        
        ! 中心から一番外側の節点までの距離を半径とする
        max = 0
        do i = 1, size
            dot = positions(i,:) - center
            tmp = DOT_PRODUCT(dot, dot)
            if (tmp > max) then
                max = tmp
            end if
        end do
        
        r = SQRT(max)
        
    end function
    
    ! コンストラクタ
    type(Coil) function init_Coil(conf, coilCount) result(this)
        use ConfigClass
        use Math
        implicit none
        type(Config), intent(in) :: conf
        integer, intent(in) :: coilCount
        real, dimension(3) :: topCenter, bottomCenter, center, unit
        integer timeid
        
        ! ファイルの読み込み
        this%input = init_InputFile(conf%inputFD, conf%coilPartNames(coilCount))
        this%top = init_ReportFile(conf%topFDs(coilCount), this%input)
        this%bottom = init_ReportFile(conf%bottomFDs(coilCount), this%input)
        
        ! 半径，高さなどを計算する
        topCenter = Coil_CenterPosition(SIZE(this%top%nodeIds), this%top%positions(1,:,:))
        bottomCenter = Coil_CenterPosition(SIZE(this%bottom%nodeIds), this%bottom%positions(1,:,:))
        center = (topCenter - bottomCenter) * 0.5 + bottomCenter
        this%height = Coil_Height(topCenter, bottomCenter)
        this%radius = Coil_Radius(SIZE(this%top%nodeIds), this%top%positions(1,:,:), topCenter)
        
        ! 時間ごとの向きを計算する
        ALLOCATE (this%forward(SIZE(this%top%times), 3))
        ALLOCATE (this%right(SIZE(this%top%times), 3))
        
        ! 正面を計算する
        do timeid = 1, SIZE(this%top%times)
            topCenter = Coil_CenterPosition(SIZE(this%top%nodeIds), this%top%positions(timeid,:,:))
            bottomCenter = Coil_CenterPosition(SIZE(this%bottom%nodeIds), this%bottom%positions(timeid,:,:))
            center = topCenter - bottomCenter
            this%forward(timeid,:) = center / SQRT(DOT_PRODUCT(center, center)) ! 単位ベクトル化
        end do
        
        ! 右手を計算する
        unit = (/ 1, 1, 1 /)
        unit = unit / SQRT(DOT_PRODUCT(unit, unit))
        do timeid = 1, SIZE(this%top%times)
            this%right(timeid,:) = cross(this%forward(timeid, :), unit)
        end do
        
        
    end function
    
    end module