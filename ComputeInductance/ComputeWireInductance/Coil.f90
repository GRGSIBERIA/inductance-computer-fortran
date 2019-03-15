    module CoilClass
    use ReportFileClass
    use InputFileClass
    implicit none
    
    type Coil
        real :: radius, height
        type(InputFile) :: input
        type(ReportFile) :: top, bottom
        real, dimension(3) :: topCenter, bottomCenter, center
    end type
    
    interface Coil
        module procedure :: init_Coil
    end interface
    
    contains
    
    ! 中心座標を計算する関数
    function Coil_CenterPosition(size, positions) result(center)
        integer, intent(in) :: size
        real, dimension(size,3), intent(in) :: positions
        real, dimension(3) :: center
        integer i
        
        do i = 1, size
            center = center + positions(i,:)
        end do
        center = center / real(size)
    end function
    
    ! 高さを計算する関数
    real function Coil_Height(top, bottom) result(h)
        real, dimension(3), intent(in) :: top, bottom
        
        h = SQRT(DOT_PRODUCT(top - bottom))
    end function
    
    ! 半径を計算する関数
    real function Coil_Radius(size, positions, center) result(r)
        integer, intent(in) :: size
        real, dimension(size,3), intent(in) :: positions
        real, dimension(3), intent(in) :: center
        real max, tmp
        integer i
        
        ! 中心から一番外側の節点までの距離を半径とする
        max = 0
        do i = 1, size
            tmp = SQRT(DOT_PRODUCT(positions(i,:) - center))
            if (tmp > max) then
                max = tmp
            end if
        end do
        
        r = max
        
    end function
    
    ! コンストラクタ
    type(Coil) function init_Coil(conf, coilCount) result(this)
        use ConfigClass
        implicit none
        type(Config), intent(in) :: conf
        integer, intent(in) :: coilCount
        
        ! ファイルの読み込み
        this%input = init_InputFile(conf%inputFD, conf%coilPartNames(coilCount))
        this%top = init_ReportFile(conf%topFDs(coilCount), this%input)
        this%bottom = init_ReportFile(conf%bottomFDs(coilCount), this%input)
        
        ! 半径，高さなどを計算する
        this%topCenter = Coil_CenterPosition(SIZE(this%top%nodeIds), this%top%positions(1,:,:))
        this%bottomCenter = Coil_CenterPosition(SIZE(this%bottom%nodeIds), this%bottom%positions(1,:,:))
        this%center = (this%topCenter - this%bottomCenter) * 0.5 + this%bottomCenter
        this%height = Coil_Height(this%topCenter, this%bottomCenter)
        this%radius = Coil_Radius(SIZE(this%top%nodeIds), this%top%positions(1,:,:), this%topCenter)
    end function
    
    end module