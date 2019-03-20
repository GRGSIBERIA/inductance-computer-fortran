﻿    module FluxDensity
    implicit none
    
    type DoubleQuadArgument
        real, dimension(3) :: wirePosition, coilForward, coilRight
        real :: sigma, dr, dt
    end type
    
    type RadialArgument
        real, dimension(3) :: movingUnitVector, forward, right, center, wirePosition
        real :: dtheta, dradius, wireFlux, gamma
    end type
    
    
    contains
    
    ! ベクトルを回転させる
    function RotateVector(axis, vector, dt) result(rotated)
        use Math
        implicit none
        real, dimension(3), intent(in) :: vector, axis
        real, dimension(3) :: Qi, Ri, Ai
        real, dimension(3) :: rotated
        real, intent(in) :: dt
        real sinv
        
        sinv = SIN(dt)
        
        Qi = axis * sinv
        Ri = axis * SIGN(sinv, -1.0)
        
        Ai = MulQuaternion_I(Qi, vector)
        rotated = MulQuaternion_I(Ai, Ri)
    end function
    
    ! 積分の内側を計算する
    function DoubleQuad(ntheta, nradius, coilPosition, args)
        use Math
        implicit none
        type(DoubleQuadArgument), intent(in) :: args
        integer, intent(in) :: ntheta, nradius
        real, dimension(3), intent(in) :: coilPosition
        real r, t
        
        real DoubleQuad, fracUp, fracDown
        real, dimension(3) :: Bi, fracDown_vec
        
        r = (nradius-1) * args%dr
        t = (ntheta-1) * args%dt
        
        ! 回転変換を実行する
        Bi = RotateVector(args%coilForward, args%coilRight, r, args%dt)
        
        ! ここで積分の内側の分数を計算する
        fracUp = args%dr * args%sigma
        fracDown_vec = args%dr * Bi + args%wirePosition - coilPosition
        fracDown = Length(fracDown_vec)
        fracDown = fracDown * fracDown * fracDown
        
        DoubleQuad = fracUp / fracDown * args%dr * args%dt
    end function
    
    ! あるワイヤがコイルで誘導された磁束密度を求める
    function WiredFluxDensity(timeid, wirePosition, coil_) result(wiredFlux)
        use CoilClass
        use WireClass
        implicit none
        integer, intent(in) :: timeid
        real, dimension(3) :: wirePosition
        type(Coil), intent(in) :: coil_
        
        real wiredFlux
        real, dimension(3) :: movingUnitVector
        integer ntheta, nradius
        real, dimension(coil_%numofDTheta, coil_%numofDRadius) :: densityMap
        
        type(DoubleQuadArgument) args
        real, parameter :: PI = ACOS(-1.0)      ! これでPIが出る
        
        ! 単位ベクトルを作る
        movingUnitVector = coil_%forward(timeid,:) * coil_%height * 0.5
        
        args%dt = 2.0 * PI / coil_%numofDTheta
        args%dr = coil_%radius / coil_%numofDRadius
        
        args%wirePosition = wirePosition
        args%coilForward = coil_%forward(timeid,:)
        args%coilRight = coil_%right(timeid,:)
        args%sigma = 1.0
        
        ! コイル上面と底面の差を積分する
        do ntheta = 1, coil_%numofDTheta
            do nradius = 1, coil_%numofDRadius
                densityMap(ntheta, nradius) = &
                    DoubleQuad(ntheta, nradius, coil_%center(timeid,:) + movingUnitVector, args) - &
                    DoubleQuad(ntheta, nradius, coil_%center(timeid,:) - movingUnitVector, args)
            end do
        end do
        
        wiredFlux = SUM(densityMap)
    end function
    
    function WiredFluxDensities(timeid, wire_, coils) result(wiredFluxes)
        use CoilClass
        use WireClass
        implicit none
        integer, intent(in) :: timeid
        type(Wire), intent(in) :: wire_
        type(Coil), dimension(:), intent(in) :: coils
        integer wi, ci
        real, dimension(SIZE(coils), wire_%numofNodes) :: wiredFluxesR
        real, dimension(wire_%numofNodes) :: wiredFluxes
        
        wiredFluxes = 0
        wiredFluxesR = 0
        
        !$omp parallel
        !$omp do
        do wi = 1, wire_%numofNodes
            do ci = 1, SIZE(coils)
                wiredFluxesR(ci, wi) = WiredFluxDensity(timeid, wire_%assembly%positions(timeid,wi,:), coils(ci))
            end do
        end do
        !$omp end do
        !$omp end parallel
        
        do wi = 1, wire_%numofNodes
            do ci = 1, SIZE(coils)
                wiredFluxes(wi) = wiredFluxes(wi) + wiredFluxesR(ci, wi)
            end do
        end do
        
    end function
    
    ! コイル上の位置について磁束密度を求める
    real function RadialPositionForFlux(dt, dr, arg) result(flux)
        implicit none
        type(RadialArgument) arg
        real, intent(in) :: dt, dr
        real, dimension(3) :: position, tempPos
        real fracUp, fracDown
        
        ! 右手ベクトルを回転させる
        ! 回転させた右手ベクトルをdradiusだけ延長する
        ! これがコイル上の位置に変換される
        position = RotateVector(forward, right, dt) * dr + movingUnitVector + center
        
        tempPos = arg%wirePosition - position
        fracUp = DOT_PRODUCT(arg%forward, tempPos * arg%forward
        fracUp = Length(fracUp)
        fracDown = Length(tempPos)
        fracDown = fracDown * fracDown * fracDown
        flux = gamma * arg%wireFlux * (fracUp / fracDown)
    end function
    
    ! コイルについて放射状に積分する
    real function RadialPointFluxes(timeid, wirePosition, wireFlux, gamma, coil_) result(flux)
        implicit none
        real, intent(in) :: wireFlux, gamma
        real, dimension(3), intent(in) :: wirePosition
        integer, intent(in) :: timeid
        type(Coil), intent(in) :: coil_
        real, parameter :: PI = ACOS(-1.0)
        real, dimension(coil_%numofDTheta, coil_%numofDRadius) :: fluxes
        real, dimension(3) :: movingUnitVector  ! 上面と下面を決めるためのベクトル
        real dradius, dtheta, flux
        type(RadialArgument) radarg
        integer ri, ti
        
        radarg%dtheta = 2.0 * PI / coil_%numofDTheta
        radarg%dradius = coil_%radius / coil_%numofDRadius
        radarg%movingUnitVector = coil_%forward(timeid,:) * coil_%height * 0.5
        radarg%forward = coil_%forward(timeid,:)
        radarg%right = coil_%right(timeid,:)
        radarg%center = coil_%center(timeid,:)
        radarg%wirePosition = wirePosition
        radarg%wireFlux = wireFlux
        radarg%gamma = gamma
        
        !$omp parallel
        !$omp do
        do ri = 1, coil_%numofDRadius
            do ti = 1, coil_%numofDTheta
                fluxes(ti, ri) = RadialPositionForFlux((ti - 1) * dtheta, (ri - 1) * dradius, radarg)
            end do
        end do
        !$omp end do
        !$omp end parallel
        
        flux = SUM(fluxes)
    end function
    
    ! コイル上面の磁束密度から誘導起電力を求める
    ! そもそもコイルの底面の磁束密度を引く必要があるのか先生に聞かないとだめかもしれない
    subroutine RadialFlux(timeid, wire_, coils, gamma) 
        use CoilClass
        use WireClass
        implicit none
        type(Wire), intent(in) :: wire_
        type(Coil), dimension(:), intent(in) :: coils
        real, dimension(SIZE(coils), SIZE(wires)) :: fluxes
        integer ci, wi, i
        
        ! コイル上の磁束密度をまとめる
        do wi = 1, wire_%numofNodes
            do ci = 1, SIZE(coils)
                fluxes(ci, wi) = RadialPointFluxes(timeid, wire_%assembly%position(timeid, wi, :), wire_%fluxes(timeid, :), gamma, coils(ci))
            end do
        end do
        
        ! コイル単位でまとめる
        ! flux自体はコンストラクタで初期化したから大丈夫
        do wi = 1, wire_%numofNodes
            do ci = 1, SIZE(coils)
                coils(ci)%flux = coils(ci)%flux + fluxes(ci, wi)
            end do
        end do
        
    end subroutine
    
    end module