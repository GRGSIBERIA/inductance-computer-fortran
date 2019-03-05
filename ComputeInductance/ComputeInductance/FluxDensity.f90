    module FluxDensity
    implicit none
    
    contains
    ! 積分の内側を計算する
    function DoubleQuad(r, t, dr, dt, wirePosition, coilPosition, coilForward, coilRight, sigma)
        use Math
        implicit none
        real, intent(in) :: dr, dt, r, t, sigma
        real, dimension(3), intent(in) :: wirePosition, coilPosition, coilForward, coilRight
        real DoubleQuad, cosv, sinv, Qr, Rr, Pr, Ar, fracUp, fracDown
        real, dimension(3) :: Qi, Ri, Pi, Ai, Bi, fracDown_vec
        
        ! 回転変換を実行する
        cosv = COS(dt)
        sinv = SIN(dt)
        
        Qi = coilForward * sinv
        Qr = cosv
        Ri = coilForward * SIGN(sinv, -1.0)
        Rr = cosv
        Pi = coilRight
        Pr = 0.0
        
        Ar = MulQuaternion_R(Qr, Qi, Pr, Pi)
        Ai = MulQuaternion_I(Qr, Qi, Pr, Pi)
        Bi = MulQuaternion_I(Ar, Ai, Rr, Ri)
        
        ! ここで積分の内側の分数を計算する
        fracUp = dr * sigma
        fracDown_vec = dr * Bi + wirePosition - coilPosition
        fracDown = Length(fracDown_vec)
        fracDown = fracDown * fracDown * fracDown
        
        DoubleQuad = fracUp / fracDown * dr * dt
    end function
    
    function WiredFluxDensity(wirePosition, coilPosition, coilForward, coilRight, coilHeight, coilRadius, sigma, numofDTheta, numofDRadius) result(wire)
        implicit none
        real, dimension(3), intent(in) :: wirePosition, coilPosition, coilForward, coilRight
        real, intent(in) :: coilHeight, coilRadius, sigma
        integer, intent(in) :: numofDTheta, numofDRadius
        real wire, dTheta, dRadius
        real, dimension(3) :: movingUnitVector
        integer ntheta, nradius
        real, dimension(numofDTheta, numofDRadius) :: densityMap
        real, parameter :: PI = ACOS(-1.0)      ! これでPIが出る
        
        ! 単位ベクトルを作る
        movingUnitVector = coilForward * coilHeight * 0.5
        dTheta = 2.0 * PI / numofDTheta
        dRadius = coilRadius_numofDRadius
        
        ! コイル上面と底面の差を積分する
        DO ntheta = 1, numofDTheta
            DO nradius = 1, numofDRadius
                densityMap(ntheta, nradius) = &
                    DoubleQuad((nradius-1) * dRadius, (ntheta-1) * dTheta, dRadius, dTheta, &  ! 上面
                        wirePosition, coilPosition + movingUnitVector, coilForward, coilRight, sigma) - &
                    DoubleQuad((nradius-1) * dRadius, (ntheta-1) * dTheta, dRadius, dTheta, &  ! 底面
                        wirePosition, coilPosition - movingUnitVector, coilForward, coilRight, sigma)
            END DO
        END DO
        
        WiredFluxDensity = SUM(densityMap)
    end function
    
    function flux_densities
    
    end module