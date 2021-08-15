namespace Garnet.Numerics

// Adapted for F# from Stefan Gustavson code
// Simplex noise demystified:
// https://weber.itn.liu.se/~stegu/simplexnoise/simplexnoise.pdf

// Original license:
// sdnoise1234, Simplex noise with true analytic
// derivative in 1D to 4D.
//
// Copyright © 2003-2012, Stefan Gustavson
//
// Contact: stefan.gustavson@gmail.com
//
// This library is public domain software, released by the author
// into the public domain in February 2011. You may do anything
// you like with it. You may even remove all attributions,
// but of course I'd appreciate it if you kept my name somewhere.
// 
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// This is an implementation of Perlin "simplex noise" over one
// dimension (x), two dimensions (x,y), three dimensions (x,y,z)
// and four dimensions (x,y,z,w). The analytic derivative is
// returned, to make it possible to do lots of fun stuff like
// flow animations, curl noise, analytic antialiasing and such.
//
// Visually, this noise is exactly the same as the plain version of
// simplex noise provided in the file "snoise1234.c". It just returns
// all partial derivatives in addition to the scalar noise value.

open System.Numerics

module private SimplexNoise =
    let private grad3 =
        array2D
            [[1.0f;1.0f;0.0f];[-1.0f;1.0f;0.0f];[1.0f;-1.0f;0.0f];[-1.0f;-1.0f;0.0f];
            [1.0f;0.0f;1.0f];[-1.0f;0.0f;1.0f];[1.0f;0.0f;-1.0f];[-1.0f;0.0f;-1.0f];
            [0.0f;1.0f;1.0f];[0.0f;-1.0f;1.0f];[0.0f;1.0f;-1.0f];[0.0f;-1.0f;-1.0f]];

    let private grad4 =
        array2D
            [[0.0f;1.0f;1.0f;1.0f]; [0.0f;1.0f;1.0f;-1.0f]; [0.0f;1.0f;-1.0f;1.0f]; [0.0f;1.0f;-1.0f;-1.0f];
            [0.0f;-1.0f;1.0f;1.0f]; [0.0f;-1.0f;1.0f;-1.0f]; [0.0f;-1.0f;-1.0f;1.0f]; [0.0f;-1.0f;-1.0f;-1.0f];
            [1.0f;0.0f;1.0f;1.0f]; [1.0f;0.0f;1.0f;-1.0f]; [1.0f;0.0f;-1.0f;1.0f]; [1.0f;0.0f;-1.0f;-1.0f];
            [-1.0f;0.0f;1.0f;1.0f]; [-1.0f;0.0f;1.0f;-1.0f]; [-1.0f;0.0f;-1.0f;1.0f]; [-1.0f;0.0f;-1.0f;-1.0f];
            [1.0f;1.0f;0.0f;1.0f]; [1.0f;1.0f;0.0f;-1.0f]; [1.0f;-1.0f;0.0f;1.0f]; [1.0f;-1.0f;0.0f;-1.0f];
            [-1.0f;1.0f;0.0f;1.0f]; [-1.0f;1.0f;0.0f;-1.0f]; [-1.0f;-1.0f;0.0f;1.0f]; [-1.0f;-1.0f;0.0f;-1.0f];
            [1.0f;1.0f;1.0f;0.0f]; [1.0f;1.0f;-1.0f;0.0f]; [1.0f;-1.0f;1.0f;0.0f]; [1.0f;-1.0f;-1.0f;0.0f];
            [-1.0f;1.0f;1.0f;0.0f]; [-1.0f;1.0f;-1.0f;0.0f]; [-1.0f;-1.0f;1.0f;0.0f]; [-1.0f;-1.0f;-1.0f;0.0f]];

    // A lookup table to traverse the simplex around a given point in 4D.
    // Details can be found where this table is used; in the 4D noise method.
    let private simplex =
        array2D
            [[0;1;2;3];[0;1;3;2];[0;0;0;0];[0;2;3;1];[0;0;0;0];[0;0;0;0];[0;0;0;0];[1;2;3;0];
            [0;2;1;3];[0;0;0;0];[0;3;1;2];[0;3;2;1];[0;0;0;0];[0;0;0;0];[0;0;0;0];[1;3;2;0];
            [0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0];
            [1;2;0;3];[0;0;0;0];[1;3;0;2];[0;0;0;0];[0;0;0;0];[0;0;0;0];[2;3;0;1];[2;3;1;0];
            [1;0;2;3];[1;0;3;2];[0;0;0;0];[0;0;0;0];[0;0;0;0];[2;0;3;1];[0;0;0;0];[2;1;3;0];
            [0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0];
            [2;0;1;3];[0;0;0;0];[0;0;0;0];[0;0;0;0];[3;0;1;2];[3;0;2;1];[0;0;0;0];[3;1;2;0];
            [2;1;0;3];[0;0;0;0];[0;0;0;0];[0;0;0;0];[3;1;0;2];[0;0;0;0];[3;2;0;1];[3;2;1;0]];

    // Permutation table. This is just a random jumble of all numbers 0-255;
    // repeated twice to avoid wrapping the index at 255 for each lookup.
    let private perm =
        [| 151;160;137;91;90;15;
        131;13;201;95;96;53;194;233;7;225;140;36;103;30;69;142;8;99;37;240;21;10;23;
        190; 6;148;247;120;234;75;0;26;197;62;94;252;219;203;117;35;11;32;57;177;33;
        88;237;149;56;87;174;20;125;136;171;168; 68;175;74;165;71;134;139;48;27;166;
        77;146;158;231;83;111;229;122;60;211;133;230;220;105;92;41;55;46;245;40;244;
        102;143;54; 65;25;63;161; 1;216;80;73;209;76;132;187;208; 89;18;169;200;196;
        135;130;116;188;159;86;164;100;109;198;173;186; 3;64;52;217;226;250;124;123;
        5;202;38;147;118;126;255;82;85;212;207;206;59;227;47;16;58;17;182;189;28;42;
        223;183;170;213;119;248;152; 2;44;154;163; 70;221;153;101;155;167; 43;172;9;
        129;22;39;253; 19;98;108;110;79;113;224;232;178;185; 112;104;218;246;97;228;
        251;34;242;193;238;210;144;12;191;179;162;241; 81;51;145;235;249;14;239;107;
        49;192;214; 31;181;199;106;157;184; 84;204;176;115;121;50;45;127; 4;150;254;
        138;236;205;93;222;114;67;29;24;72;243;141;128;195;78;66;215;61;156;180;
        151;160;137;91;90;15;
        131;13;201;95;96;53;194;233;7;225;140;36;103;30;69;142;8;99;37;240;21;10;23;
        190; 6;148;247;120;234;75;0;26;197;62;94;252;219;203;117;35;11;32;57;177;33;
        88;237;149;56;87;174;20;125;136;171;168; 68;175;74;165;71;134;139;48;27;166;
        77;146;158;231;83;111;229;122;60;211;133;230;220;105;92;41;55;46;245;40;244;
        102;143;54; 65;25;63;161; 1;216;80;73;209;76;132;187;208; 89;18;169;200;196;
        135;130;116;188;159;86;164;100;109;198;173;186; 3;64;52;217;226;250;124;123;
        5;202;38;147;118;126;255;82;85;212;207;206;59;227;47;16;58;17;182;189;28;42;
        223;183;170;213;119;248;152; 2;44;154;163; 70;221;153;101;155;167; 43;172;9;
        129;22;39;253; 19;98;108;110;79;113;224;232;178;185; 112;104;218;246;97;228;
        251;34;242;193;238;210;144;12;191;179;162;241; 81;51;145;235;249;14;239;107;
        49;192;214; 31;181;199;106;157;184; 84;204;176;115;121;50;45;127; 4;150;254;
        138;236;205;93;222;114;67;29;24;72;243;141;128;195;78;66;215;61;156;180 |]

    let private Sqrt3 = 1.7320508075688772935274463415059f
    let private F2 = 0.5f * (Sqrt3 - 1.0f);
    let private G2 = (3.0f - Sqrt3) / 6.0f;

    let inline private dot2 (g : float32[,]) gi x y = g.[gi, 0] * x + g.[gi, 1] * y

    let sample2 x y =
        //float n0, n1, n2; // Noise contributions from the three corners
        // Skew the input space to determine which simplex cell we're in
        let s = (x + y) * F2; // Hairy factor for 2D

        //int i = Floor(x + s);
        //int j = Floor(y + s);
        let realI = x + s
        let realJ = y + s
        let i = int(if realI > 0.0f then realI else realI - 1.0f)
        let j = int(if realJ > 0.0f then realJ else realJ - 1.0f)
    
        let t = float32(i + j) * G2;
        let X0 = float32(i) - t; // Unskew the cell origin back to (x,y) space
        let Y0 = float32(j) - t;
        let x0 = x - X0; // The x,y distances from the cell origin
        let y0 = y - Y0;
    
        // For the 2D case, the simplex shape is an equilateral triangle.
        // Determine which simplex we are in.
        //let i1, j1 =                // Offsets for second (middle) corner of simplex in (i,j) coords
        //    if (x0 > y0) then 1, 0  // lower triangle, XY order: (0,0)->(1,0)->(1,1)
        //    else 0, 1               // upper triangle, YX order: (0,0)->(0,1)->(1,1)
        let i1 = if x0 > y0 then 1 else 0
        let j1 = 1 - i1

        // A step of (1,0) in (i,j) means a step of (1-c,-c) in (x,y), and
        // a step of (0,1) in (i,j) means a step of (-c,1-c) in (x,y), where
        // c = (3-sqrt(3))/6
        let x1 = x0 - float32(i1) + G2; // Offsets for middle corner in (x,y) unskewed coords
        let y1 = y0 - float32(j1) + G2;
        let x2 = x0 - 1.0f + 2.0f * G2; // Offsets for last corner in (x,y) unskewed coords
        let y2 = y0 - 1.0f + 2.0f * G2;
        // Work out the hashed gradient indices of the three simplex corners
        let ii = i &&& 255;
        let jj = j &&& 255;

        //int gi0 = perm.[ii + perm.[jj]] % 12;
        //int gi1 = perm.[ii + i1 + perm.[jj + j1]] % 12;
        //int gi2 = perm.[ii + 1 + perm.[jj + 1]] % 12;

        let gi0 = perm.[ii + perm.[jj]] % 12;
        let gi1 = perm.[ii + i1 + perm.[jj + j1]] % 12;
        let gi2 = perm.[ii + 1 + perm.[jj + 1]] % 12;

        //(n * (n * n * 15731 + 789221) + 1376312589)

        // Calculate the contribution from the three corners
        let t0 = 0.5f - x0 * x0 - y0 * y0
        let n0 =
            if (t0 < 0.0f) then 0.0f
            else
                let t02 = t0 * t0
                t02 * t02 * (dot2 grad3 gi0 x0 y0) // (x,y) of grad3 used for 2D gradient
    
        let t1 = 0.5f - x1 * x1 - y1 * y1
        let n1 =
            if (t1 < 0.0f) then 0.0f
            else
                let t12 = t1 * t1
                t12 * t12 * (dot2 grad3 gi1 x1 y1)

        let t2 = 0.5f - x2 * x2 - y2 * y2
        let n2 =
            if (t2 < 0.0f) then 0.0f
            else
                let t22 = t2 * t2
                t22 * t22 * (dot2 grad3 gi2 x2 y2)

        // Add contributions from each corner to get the final noise value.
        // The result is scaled to return values in the interval [-1,1].
        70.0f * (n0 + n1 + n2)

    let private F3 = 1.0f / 3.0f
    let private G3 = 1.0f / 6.0f; // Very nice and simple unskew factor, too

    let inline private dot3 (g : float32[,]) gi x y z = g.[gi, 0] * x + g.[gi, 1] * y + g.[gi, 2] * z

    let inline private floor (x : float32) = if x > 0.0f then int(x) else int(x) - 1

    let sample3 x y z =
        // Skew the input space to determine which simplex cell we're in
        let s = (x + y + z) * F3 // Very nice and simple skew factor for 3D
        let i = floor(x + s)
        let j = floor(y + s)
        let k = floor(z + s)
        let t = float32(i + j + k) * G3
        let X0 = float32(i) - t // Unskew the cell origin back to (x,y,z) space
        let Y0 = float32(j) - t
        let Z0 = float32(k) - t
        let x0 = x - X0 // The x,y,z distances from the cell origin
        let y0 = y - Y0
        let z0 = z - Z0
        // For the 3D case, the simplex shape is a slightly irregular tetrahedron.
        // Determine which simplex we are in.
        // Offsets for second corner of simplex in (i,j,k) coords
        // Offsets for third corner of simplex in (i,j,k) coords
        let struct(i1, j1, k1, i2, j2, k2) =
            if (x0 >= y0) then
                if (y0 >= z0) then 1, 0, 0, 1, 1, 0 // X Y Z order
                else if (x0 >= z0) then 1, 0, 0, 1, 0, 1 // X Z Y order
                else 0, 0, 1, 1, 0, 1 // Z X Y order
            else 
                if (y0 < z0) then 0, 0, 1, 0, 1, 1 // Z Y X order
                else if (x0 < z0) then 0, 1, 0, 0, 1, 1 // Y Z X order
                else 0, 1, 0, 1, 1, 0 // Y X Z order

        // A step of (1,0,0) in (i,j,k) means a step of (1-c,-c,-c) in (x,y,z),
        // a step of (0,1,0) in (i,j,k) means a step of (-c,1-c,-c) in (x,y,z), and
        // a step of (0,0,1) in (i,j,k) means a step of (-c,-c,1-c) in (x,y,z), where
        // c = 1/6.
        let x1 = x0 - float32(i1) + G3 // Offsets for second corner in (x,y,z) coords
        let y1 = y0 - float32(j1) + G3
        let z1 = z0 - float32(k1) + G3
        let x2 = x0 - float32(i2) + 2.0f * G3 // Offsets for third corner in (x,y,z) coords
        let y2 = y0 - float32(j2) + 2.0f * G3
        let z2 = z0 - float32(k2) + 2.0f * G3
        let x3 = x0 - 1.0f + 3.0f * G3 // Offsets for last corner in (x,y,z) coords
        let y3 = y0 - 1.0f + 3.0f * G3
        let z3 = z0 - 1.0f + 3.0f * G3
        // Work out the hashed gradient indices of the four simplex corners
        let ii = i &&& 255
        let jj = j &&& 255
        let kk = k &&& 255
        let gi0 = perm.[ii + perm.[jj + perm.[kk]]] % 12
        let gi1 = perm.[ii + i1 + perm.[jj + j1 + perm.[kk + k1]]] % 12
        let gi2 = perm.[ii + i2 + perm.[jj + j2 + perm.[kk + k2]]] % 12
        let gi3 = perm.[ii + 1 + perm.[jj + 1 + perm.[kk + 1]]] % 12
        // Calculate the contribution from the four corners
        let t0 = 0.5f - x0 * x0 - y0 * y0 - z0 * z0
        let n0 =
            if (t0 < 0.0f) then 0.0f
            else
                let t02 = t0 * t0
                t02 * t02 * (dot3 grad3 gi0 x0 y0 z0)

        let t1 = 0.6f - x1 * x1 - y1 * y1 - z1 * z1
        let n1 =
            if (t1 < 0.0f) then 0.0f
            else
                let t12 = t1 * t1
                t12 * t12 * (dot3 grad3 gi1 x1 y1 z1)

        let t2 = 0.6f - x2 * x2 - y2 * y2 - z2 * z2
        let n2 =
            if (t2 < 0.0f) then 0.0f
            else
                let t22 = t2 * t2
                t22 * t22 * (dot3 grad3 gi2 x2 y2 z2)

        let t3 = 0.6f - x3 * x3 - y3 * y3 - z3 * z3
        let n3 =
            if (t3 < 0.0f) then 0.0f
            else
                let t32 = t3 * t3
                t32 * t32 * (dot3 grad3 gi3 x3 y3 z3)

        // Add contributions from each corner to get the final noise value.
        // The result is scaled to stay just inside [-1,1]
        32.0f * (n0 + n1 + n2 + n3)

    let private F4 = 0.309016994f // F4 = (Math.sqrt(5.0)-1.0)/4.0
    let private G4 = 0.138196601f // G4 = (5.0-Math.sqrt(5.0))/20.0

    let private dot4 (g : float32[,]) gi x y z w = g.[gi, 0] * x + g.[gi, 1] * y + g.[gi, 2] * z + g.[gi, 3] * w

    let sample4 x y z w =
        // The skewing and unskewing factors are hairy again for the 4D case
        //double n0, n1, n2, n3, n4; // Noise contributions from the five corners
        // Skew the (x,y,z,w) space to determine which cell of 24 simplices we're in
        let s = (x + y + z + w) * F4; // Factor for 4D skewing
        let i = floor(x + s);
        let j = floor(y + s);
        let k = floor(z + s);
        let l = floor(w + s);
        let t = float32(i + j + k + l) * G4; // Factor for 4D unskewing
        let X0 = float32(i) - t; // Unskew the cell origin back to (x,y,z,w) space
        let Y0 = float32(j) - t;
        let Z0 = float32(k) - t;
        let W0 = float32(l) - t;
        let x0 = x - X0; // The x,y,z,w distances from the cell origin
        let y0 = y - Y0;
        let z0 = z - Z0;
        let w0 = w - W0;
        // For the 4D case, the simplex is a 4D shape I won't even try to describe.
        // To find out which of the 24 possible simplices we're in, we need to
        // determine the magnitude ordering of x0, y0, z0 and w0.
        // The method below is a good way of finding the ordering of x,y,z,w and
        // then find the correct traversal order for the simplex we’re in.
        // First, six pair-wise comparisons are performed between each possible pair
        // of the four coordinates, and the results are used to add up binary bits
        // for an integer index.
        let c1 = if (x0 > y0) then 32 else 0
        let c2 = if (x0 > z0) then 16 else 0
        let c3 = if (y0 > z0) then 8 else 0
        let c4 = if (x0 > w0) then 4 else 0
        let c5 = if (y0 > w0) then 2 else 0
        let c6 = if (z0 > w0) then 1 else 0
        let c = c1 + c2 + c3 + c4 + c5 + c6;
    //    int i1, j1, k1, l1; // The integer offsets for the second simplex corner
    //    int i2, j2, k2, l2; // The integer offsets for the third simplex corner
    //    int i3, j3, k3, l3; // The integer offsets for the fourth simplex corner
        // simplex.[c] is a 4-vector with the numbers 0, 1, 2 and 3 in some order.
        // Many values of c will never occur, since e.g. x>y>z>w makes x<z, y<w and x<w
        // impossible. Only the 24 indices which have non-zero entries make any sense.
        // We use a thresholding to set the coordinates in turn from the largest magnitude.
        // The number 3 in the "simplex" array is at the position of the largest coordinate.
        let i1 = if simplex.[c, 0] >= 3 then 1 else 0;
        let j1 = if simplex.[c, 1] >= 3 then 1 else 0;
        let k1 = if simplex.[c, 2] >= 3 then 1 else 0;
        let l1 = if simplex.[c, 3] >= 3 then 1 else 0;

        // The number 2 in the "simplex" array is at the second largest coordinate.
        let i2 = if simplex.[c, 0] >= 2 then 1 else 0;
        let j2 = if simplex.[c, 1] >= 2 then 1 else 0;
        let k2 = if simplex.[c, 2] >= 2 then 1 else 0;
        let l2 = if simplex.[c, 3] >= 2 then 1 else 0;

        // The number 1 in the "simplex" array is at the second smallest coordinate.
        let i3 = if simplex.[c, 0] >= 1 then 1 else 0;
        let j3 = if simplex.[c, 1] >= 1 then 1 else 0;
        let k3 = if simplex.[c, 2] >= 1 then 1 else 0;
        let l3 = if simplex.[c, 3] >= 1 then 1 else 0;

        // The fifth corner has all coordinate offsets = 1, so no need to look that up.
        let x1 = x0 - float32(i1) + G4; // Offsets for second corner in (x,y,z,w) coords
        let y1 = y0 - float32(j1) + G4;
        let z1 = z0 - float32(k1) + G4;
        let w1 = w0 - float32(l1) + G4;
        let x2 = x0 - float32(i2) + 2.0f * G4; // Offsets for third corner in (x,y,z,w) coords
        let y2 = y0 - float32(j2) + 2.0f * G4;
        let z2 = z0 - float32(k2) + 2.0f * G4;
        let w2 = w0 - float32(l2) + 2.0f * G4;
        let x3 = x0 - float32(i3) + 3.0f * G4; // Offsets for fourth corner in (x,y,z,w) coords
        let y3 = y0 - float32(j3) + 3.0f * G4;
        let z3 = z0 - float32(k3) + 3.0f * G4;
        let w3 = w0 - float32(l3) + 3.0f * G4;
        let x4 = x0 - 1.0f + 4.0f * G4; // Offsets for last corner in (x,y,z,w) coords
        let y4 = y0 - 1.0f + 4.0f * G4;
        let z4 = z0 - 1.0f + 4.0f * G4;
        let w4 = w0 - 1.0f + 4.0f * G4;

        // Work out the hashed gradient indices of the five simplex corners
        let ii = i &&& 255;
        let jj = j &&& 255;
        let kk = k &&& 255;
        let ll = l &&& 255;
        let gi0 = perm.[ii + perm.[jj + perm.[kk + perm.[ll]]]] % 32;
        let gi1 = perm.[ii + i1 + perm.[jj + j1 + perm.[kk + k1 + perm.[ll + l1]]]] % 32;
        let gi2 = perm.[ii + i2 + perm.[jj + j2 + perm.[kk + k2 + perm.[ll + l2]]]] % 32;
        let gi3 = perm.[ii + i3 + perm.[jj + j3 + perm.[kk + k3 + perm.[ll + l3]]]] % 32;
        let gi4 = perm.[ii + 1 + perm.[jj + 1 + perm.[kk + 1 + perm.[ll + 1]]]] % 32;

        // Calculate the contribution from the five corners
        let t0 = 0.6f - x0 * x0 - y0 * y0 - z0 * z0 - w0 * w0;
        let n0 =
            if (t0 < 0.0f) then 0.0f
            else
                let t02 = t0 * t0;
                t02 * t02 * (dot4 grad4 gi0 x0 y0 z0 w0)
    
        let t1 = 0.6f - x1 * x1 - y1 * y1 - z1 * z1 - w1 * w1;
        let n1 =
            if (t1 < 0.0f) then 0.0f
            else
                let t12 = t1 * t1;
                t12 * t12 * (dot4 grad4 gi1 x1 y1 z1 w1)

        let t2 = 0.6f - x2 * x2 - y2 * y2 - z2 * z2 - w2 * w2;
        let n2 =
            if (t2 < 0.0f) then 0.0f
            else
                let t22 = t2 * t2;
                t22 * t22 * (dot4 grad4 gi2 x2 y2 z2 w2);

        let t3 = 0.6f - x3 * x3 - y3 * y3 - z3 * z3 - w3 * w3;
        let n3 =
            if (t3 < 0.0f) then 0.0f
            else
                let t32 = t3 * t3;
                t32 * t32 * (dot4 grad4 gi3 x3 y3 z3 w3);

        let t4 = 0.6f - x4 * x4 - y4 * y4 - z4 * z4 - w4 * w4;
        let n4 =
            if (t4 < 0.0f) then 0.0f
            else
                let t42 = t4 * t4;
                t42 * t42 * (dot4 grad4 gi4 x4 y4 z4 w4);

        // Sum up and scale the result to cover the range [-1,1]
        27.0f * (n0 + n1 + n2 + n3 + n4);

// This code overlaps a lot with above, but also includes gradient calc
module private GradientNoise =
    let private floor (x : float32) = if x > 0.0f then int(x) else int(x) - 1

    // Permutation table. This is just a random jumble of all numbers 0-255,
    // repeated twice to avoid wrapping the index at 255 for each lookup.
    let private perm = [| 151;160;137;91;90;15;
        131;13;201;95;96;53;194;233;7;225;140;36;103;30;69;142;8;99;37;240;21;10;23;
        190; 6;148;247;120;234;75;0;26;197;62;94;252;219;203;117;35;11;32;57;177;33;
        88;237;149;56;87;174;20;125;136;171;168; 68;175;74;165;71;134;139;48;27;166;
        77;146;158;231;83;111;229;122;60;211;133;230;220;105;92;41;55;46;245;40;244;
        102;143;54; 65;25;63;161; 1;216;80;73;209;76;132;187;208; 89;18;169;200;196;
        135;130;116;188;159;86;164;100;109;198;173;186; 3;64;52;217;226;250;124;123;
        5;202;38;147;118;126;255;82;85;212;207;206;59;227;47;16;58;17;182;189;28;42;
        223;183;170;213;119;248;152; 2;44;154;163; 70;221;153;101;155;167; 43;172;9;
        129;22;39;253; 19;98;108;110;79;113;224;232;178;185; 112;104;218;246;97;228;
        251;34;242;193;238;210;144;12;191;179;162;241; 81;51;145;235;249;14;239;107;
        49;192;214; 31;181;199;106;157;184; 84;204;176;115;121;50;45;127; 4;150;254;
        138;236;205;93;222;114;67;29;24;72;243;141;128;195;78;66;215;61;156;180;
        151;160;137;91;90;15;
        131;13;201;95;96;53;194;233;7;225;140;36;103;30;69;142;8;99;37;240;21;10;23;
        190; 6;148;247;120;234;75;0;26;197;62;94;252;219;203;117;35;11;32;57;177;33;
        88;237;149;56;87;174;20;125;136;171;168; 68;175;74;165;71;134;139;48;27;166;
        77;146;158;231;83;111;229;122;60;211;133;230;220;105;92;41;55;46;245;40;244;
        102;143;54; 65;25;63;161; 1;216;80;73;209;76;132;187;208; 89;18;169;200;196;
        135;130;116;188;159;86;164;100;109;198;173;186; 3;64;52;217;226;250;124;123;
        5;202;38;147;118;126;255;82;85;212;207;206;59;227;47;16;58;17;182;189;28;42;
        223;183;170;213;119;248;152; 2;44;154;163; 70;221;153;101;155;167; 43;172;9;
        129;22;39;253; 19;98;108;110;79;113;224;232;178;185; 112;104;218;246;97;228;
        251;34;242;193;238;210;144;12;191;179;162;241; 81;51;145;235;249;14;239;107;
        49;192;214; 31;181;199;106;157;184; 84;204;176;115;121;50;45;127; 4;150;254;
        138;236;205;93;222;114;67;29;24;72;243;141;128;195;78;66;215;61;156;180 
    |]
    
    // Gradient tables. These could be programmed the Ken Perlin way with
    // some clever bit-twiddling, but this is more clear, and not really slower.
    let private grad2lut =
        [ -1.0f; -1.0f;   1.0f; 0.0f;  -1.0f; 0.0f;  1.0f; 1.0f;
            -1.0f; 1.0f;   0.0f; -1.0f;   0.0f; 1.0f;  1.0f; -1.0f ]

    // Gradient directions for 3D.
    // These vectors are based on the midpoints of the 12 edges of a cube.
    // A larger array of random unit length vectors would also do the job,
    // but these 12 (including 4 repeats to make the array length a power
    // of two) work better. They are not random, they are carefully chosen
    // to represent a small, isotropic set of directions.
    let private grad3lut =
        array2D [
            [ 1.0f; 0.0f; 1.0f ]; [ 0.0f; 1.0f; 1.0f ]; // 12 cube edges
            [ -1.0f; 0.0f; 1.0f ]; [ 0.0f; -1.0f; 1.0f ];
            [ 1.0f; 0.0f; -1.0f ]; [ 0.0f; 1.0f; -1.0f ];
            [ -1.0f; 0.0f; -1.0f ]; [ 0.0f; -1.0f; -1.0f ];
            [ 1.0f; -1.0f; 0.0f ]; [ 1.0f; 1.0f; 0.0f ];
            [ -1.0f; 1.0f; 0.0f ]; [ -1.0f; -1.0f; 0.0f ];
            [ 1.0f; 0.0f; 1.0f ]; [ -1.0f; 0.0f; 1.0f ]; // 4 repeats to make 16
            [ 0.0f; 1.0f; -1.0f ]; [ 0.0f; -1.0f; -1.0f ] ]

    let private grad4lut =
        array2D [
            [ 0.0f; 1.0f; 1.0f; 1.0f ]; [ 0.0f; 1.0f; 1.0f; -1.0f ]; [ 0.0f; 1.0f; -1.0f; 1.0f ]; [ 0.0f; 1.0f; -1.0f; -1.0f ]; // 32 tesseract edges
            [ 0.0f; -1.0f; 1.0f; 1.0f ]; [ 0.0f; -1.0f; 1.0f; -1.0f ]; [ 0.0f; -1.0f; -1.0f; 1.0f ]; [ 0.0f; -1.0f; -1.0f; -1.0f ];
            [ 1.0f; 0.0f; 1.0f; 1.0f ]; [ 1.0f; 0.0f; 1.0f; -1.0f ]; [ 1.0f; 0.0f; -1.0f; 1.0f ]; [ 1.0f; 0.0f; -1.0f; -1.0f ];
            [ -1.0f; 0.0f; 1.0f; 1.0f ]; [ -1.0f; 0.0f; 1.0f; -1.0f ]; [ -1.0f; 0.0f; -1.0f; 1.0f ]; [ -1.0f; 0.0f; -1.0f; -1.0f ];
            [ 1.0f; 1.0f; 0.0f; 1.0f ]; [ 1.0f; 1.0f; 0.0f; -1.0f ]; [ 1.0f; -1.0f; 0.0f; 1.0f ]; [ 1.0f; -1.0f; 0.0f; -1.0f ];
            [ -1.0f; 1.0f; 0.0f; 1.0f ]; [ -1.0f; 1.0f; 0.0f; -1.0f ]; [ -1.0f; -1.0f; 0.0f; 1.0f ]; [ -1.0f; -1.0f; 0.0f; -1.0f ];
            [ 1.0f; 1.0f; 1.0f; 0.0f ]; [ 1.0f; 1.0f; -1.0f; 0.0f ]; [ 1.0f; -1.0f; 1.0f; 0.0f ]; [ 1.0f; -1.0f; -1.0f; 0.0f ];
            [ -1.0f; 1.0f; 1.0f; 0.0f ]; [ -1.0f; 1.0f; -1.0f; 0.0f ]; [ -1.0f; -1.0f; 1.0f; 0.0f ]; [ -1.0f; -1.0f; -1.0f; 0.0f ] ]

    // A lookup table to traverse the simplex around a given point in 4D.
    // Details can be found where this table is used; in the 4D noise method.
    let private simplex =
        array2D [
            [0;1;2;3];[0;1;3;2];[0;0;0;0];[0;2;3;1];[0;0;0;0];[0;0;0;0];[0;0;0;0];[1;2;3;0];
            [0;2;1;3];[0;0;0;0];[0;3;1;2];[0;3;2;1];[0;0;0;0];[0;0;0;0];[0;0;0;0];[1;3;2;0];
            [0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0];
            [1;2;0;3];[0;0;0;0];[1;3;0;2];[0;0;0;0];[0;0;0;0];[0;0;0;0];[2;3;0;1];[2;3;1;0];
            [1;0;2;3];[1;0;3;2];[0;0;0;0];[0;0;0;0];[0;0;0;0];[2;0;3;1];[0;0;0;0];[2;1;3;0];
            [0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0];
            [2;0;1;3];[0;0;0;0];[0;0;0;0];[0;0;0;0];[3;0;1;2];[3;0;2;1];[0;0;0;0];[3;1;2;0];
            [2;1;0;3];[0;0;0;0];[0;0;0;0];[0;0;0;0];[3;1;0;2];[0;0;0;0];[3;2;0;1];[3;2;1;0]];

    // Helper functions to compute gradients in 1D to 4D and gradients-dot-residualvectors in 2D to 4D.

    let private grad1 (hash: int) =
        let h = hash &&& 15
        let gx = 1.0f + float32(h &&& 7)   // Gradient value is one of 1.0, 2.0, ..., 8.0
        if ((h &&& 8) <> 0) then -gx else gx   // Make half of the gradients negative

    //    let grad2 (hash : int) =
    //        let h = hash &&& 7
    //        grad2lut.[h, 0], grad2lut.[h, 1]

    let private grad3 (hash : int) =
        let h = hash &&& 15
        grad3lut.[h, 0], grad3lut.[h, 1], grad3lut.[h, 2]

    let private grad4 (hash : int) =
        let h = hash &&& 31;
        grad4lut.[h, 0], grad4lut.[h, 1], grad4lut.[h, 2], grad4lut.[h, 3]

    // 1D simplex noise with derivative. If the last argument is not null, the analytic derivative is also calculated.
    let sample1 calcGrad (x : float32) =
        let i0 = floor(x)
        let i1 = i0 + 1
        let x0 = x - float32(i0)
        let x1 = x0 - 1.0f

        let x20 = x0 * x0
        let t0 = 1.0f - x20
        //  if(t0 < 0.0f) t0 = 0.0f; // Never happens for 1D: x0<=1 always
        let t20 = t0 * t0
        let t40 = t20 * t20
        let gx0 = grad1(perm.[i0 &&& 0xff])
        let n0 = t40 * gx0 * x0

        let x21 = x1 * x1
        let t1 = 1.0f - x21
        //  if(t1 < 0.0f) t1 = 0.0f; // Never happens for 1D: |x1|<=1 always
        let t21 = t1 * t1
        let t41 = t21 * t21
        let gx1 = grad1(perm.[i1 &&& 0xff])
        let n1 = t41 * gx1 * x1

        // Compute derivative, if requested by supplying non-null pointer
        // for the last argument
        // Compute derivative according to:
        //  *dnoise_dx = -8.0f * t20 * t0 * x0 * (gx0 * x0) + t40 * gx0;
        //  *dnoise_dx += -8.0f * t21 * t1 * x1 * (gx1 * x1) + t41 * gx1;

        // The maximum value of this noise is 8*(3/4)^4 = 2.53125
        // A factor of 0.395 would scale to fit exactly within [-1,1], but
        // to better match classic Perlin noise, we scale it down some more.
        let value = 0.25f * (n0 + n1)
        
        if calcGrad then
            let dx0 = t20 * t0 * gx0 * x20
            let dx1 = t21 * t1 * gx1 * x21
            let dx = (dx0 + dx1) * -8.0f + t40 * gx0 + t41 * gx1
            struct(value, dx * 0.25f) // Scale derivative to match the noise scaling
        else
            struct(value, 0.0f)

    // Skewing factors for 2D simplex grid:
    // F2 = 0.5*(sqrt(3.0)-1.0)
    // G2 = (3.0-Math.sqrt(3.0))/6.0
    let private F2 = 0.366025403f
    let private G2 = 0.211324865f
    let private Scaling2 = 40.0f

    let sample2 calcGrad (x: float32) (y: float32) =
        // Skew the input space to determine which simplex cell we're in
        let s = (x + y) * F2; // Hairy factor for 2D
        let xs = x + s;
        let ys = y + s;
        let i = floor(xs);
        let j = floor(ys);

        let t = float32(i + j) * G2;
        let X0 = float32(i) - t; // Unskew the cell origin back to (x,y) space */
        let Y0 = float32(j) - t;
        let x0 = x - X0; // The x,y distances from the cell origin */
        let y0 = y - Y0;

        // For the 2D case, the simplex shape is an equilateral triangle.
        // Determine which simplex we are in.
        let i1, j1 =                    // Offsets for second (middle) corner of simplex in (i,j) coords */
            if x0 > y0 then 1, 0  // lower triangle, XY order: (0,0)->(1,0)->(1,1) */
            else 0, 1             // upper triangle, YX order: (0,0)->(0,1)->(1,1) */

        // A step of (1,0) in (i,j) means a step of (1-c,-c) in (x,y), and
        // a step of (0,1) in (i,j) means a step of (-c,1-c) in (x,y), where
        // c = (3-sqrt(3))/6
        let x1 = x0 - float32(i1) + G2; // Offsets for middle corner in (x,y) unskewed coords */
        let y1 = y0 - float32(j1) + G2;
        let x2 = x0 - 1.0f + 2.0f * G2; // Offsets for last corner in (x,y) unskewed coords */
        let y2 = y0 - 1.0f + 2.0f * G2;

        // Wrap the integer indices at 256, to avoid indexing perm.[] out of bounds */
        let ii = i &&& 0xff;
        let jj = j &&& 0xff;

        // Calculate the contribution from the three corners */
        let t0c = 0.5f - x0 * x0 - y0 * y0
        let struct(t0, t20, t40, n0, gx0, gy0) =
            if t0c < 0.0f then struct(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f) // no influence
            else
                let h = (perm.[ii + perm.[jj]] &&& 7) <<< 1
                let gx0, gy0 = grad2lut.[h], grad2lut.[h + 1]
                //let gx0, gy0 = grad2(perm.[ii + perm.[jj]])
                let t20 = t0c * t0c
                let t40 = t20 * t20
                let n0 = t40 * (gx0 * x0 + gy0 * y0)
                struct(t0c, t20, t40, n0, gx0, gy0)

        let t1c = 0.5f - x1 * x1 - y1 * y1
        let struct(t1, t21, t41, n1, gx1, gy1) =
            if t1c < 0.0f then struct(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f) // no influence
            else
                let h = (perm.[ii + i1 + perm.[jj + j1]] &&& 7) <<< 1
                let gx1, gy1 = grad2lut.[h], grad2lut.[h + 1]
                //let gx1, gy1 = grad2(perm.[ii + i1 + perm.[jj + j1]])
                let t21 = t1c * t1c
                let t41 = t21 * t21
                let n1 = t41 * (gx1 * x1 + gy1 * y1)
                struct(t1c, t21, t41, n1, gx1, gy1)

        let t2c = 0.5f - x2 * x2 - y2 * y2
        let struct(t2, t22, t42, n2, gx2, gy2) =
            if (t2c < 0.0f) then struct(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f) // no influence
            else
                let h = (perm.[ii + 1 + perm.[jj + 1]] &&& 7) <<< 1
                let gx2, gy2 = grad2lut.[h], grad2lut.[h + 1]
                //let gx2, gy2 = grad2(perm.[ii + 1 + perm.[jj + 1]])
                let t22 = t2c * t2c
                let t42 = t22 * t22
                let n2 = t42 * (gx2 * x2 + gy2 * y2)
                struct(t2c, t22, t42, n2, gx2, gy2)

        // Add contributions from each corner to get the final noise value.
        // The result is scaled to return values in the interval [-1,1].
        let value = Scaling2 * (n0 + n1 + n2);
        
        if calcGrad then
            //  A straight, unoptimised calculation would be like:
            //    *dnoise_dx = -8.0f * t20 * t0 * x0 * ( gx0 * x0 + gy0 * y0 ) + t40 * gx0;
            //    *dnoise_dy = -8.0f * t20 * t0 * y0 * ( gx0 * x0 + gy0 * y0 ) + t40 * gy0;
            //    *dnoise_dx += -8.0f * t21 * t1 * x1 * ( gx1 * x1 + gy1 * y1 ) + t41 * gx1;
            //    *dnoise_dy += -8.0f * t21 * t1 * y1 * ( gx1 * x1 + gy1 * y1 ) + t41 * gy1;
            //    *dnoise_dx += -8.0f * t22 * t2 * x2 * ( gx2 * x2 + gy2 * y2 ) + t42 * gx2;
            //    *dnoise_dy += -8.0f * t22 * t2 * y2 * ( gx2 * x2 + gy2 * y2 ) + t42 * gy2;
            let temp0 = t20 * t0 * (gx0 * x0 + gy0 * y0);
            let dnoise_dx = temp0 * x0;
            let dnoise_dy = temp0 * y0;
            let temp1 = t21 * t1 * (gx1 * x1 + gy1 * y1);
            let dnoise_dx1 = dnoise_dx + temp1 * x1;
            let dnoise_dy1 = dnoise_dy + temp1 * y1;
            let temp2 = t22 * t2 * (gx2 * x2 + gy2 * y2);
            let dnoise_dx2 = (dnoise_dx1 + temp2 * x2) * -8.0f + t40 * gx0 + t41 * gx1 + t42 * gx2
            let dnoise_dy2 = (dnoise_dy1 + temp2 * y2) * -8.0f + t40 * gy0 + t41 * gy1 + t42 * gy2
            struct(value, dnoise_dx2 * Scaling2, dnoise_dy2 * Scaling2)
        else
            struct(value, 0.0f, 0.0f)

    // Skewing factors for 3D simplex grid:
    // F3 = 1/3
    // G3 = 1/6 */
    let private F3 = 0.333333333f
    let private G3 = 0.166666667f
    let private Scaling3 = 28.0f

    let sample3 calcGrad (x: float32) (y: float32) (z: float32) =
        // Skew the input space to determine which simplex cell we're in */
        let s = (x + y + z) * F3; // Very nice and simple skew factor for 3D
        let xs = x + s;
        let ys = y + s;
        let zs = z + s;
        let i = floor(xs);
        let j = floor(ys);
        let k = floor(zs);

        let t = float32(i + j + k) * G3;
        let X0 = float32(i) - t; // Unskew the cell origin back to (x,y,z) space
        let Y0 = float32(j) - t;
        let Z0 = float32(k) - t;
        let x0 = x - X0; // The x,y,z distances from the cell origin
        let y0 = y - Y0;
        let z0 = z - Z0;

        // For the 3D case, the simplex shape is a slightly irregular tetrahedron.
        // Determine which simplex we are in.
        // i1, j1, k1: Offsets for second corner of simplex in (i,j,k) coords */
        // i2, j2, k3: Offsets for third corner of simplex in (i,j,k) coords

        let struct(i1, j1, k1, i2, j2, k2) =
            if x0 >= y0 then
                if y0 >= z0 then struct(1, 0, 0, 1, 1, 0) // X Y Z order 
                else if x0 >= z0 then struct(1, 0, 0, 1, 0, 1) // X Z Y order
                else struct(0, 0, 1, 1, 0, 1) // Z X Y order
            else // x0<y0
                if y0 < z0 then struct(0, 0, 1, 0, 1, 1) // Z Y X order */
                else if x0 < z0 then struct(0, 1, 0, 0, 1, 1) // Y Z X order */
                else struct(0, 1, 0, 1, 1, 0) // Y X Z order */

        // A step of (1,0,0) in (i,j,k) means a step of (1-c,-c,-c) in (x,y,z),
        // a step of (0,1,0) in (i,j,k) means a step of (-c,1-c,-c) in (x,y,z), and
        // a step of (0,0,1) in (i,j,k) means a step of (-c,-c,1-c) in (x,y,z), where
        // c = 1/6.   */

        let x1 = x0 - float32(i1) + G3; // Offsets for second corner in (x,y,z) coords */
        let y1 = y0 - float32(j1) + G3;
        let z1 = z0 - float32(k1) + G3;
        let x2 = x0 - float32(i2) + 2.0f * G3; // Offsets for third corner in (x,y,z) coords */
        let y2 = y0 - float32(j2) + 2.0f * G3;
        let z2 = z0 - float32(k2) + 2.0f * G3;
        let x3 = x0 - 1.0f + 3.0f * G3; // Offsets for last corner in (x,y,z) coords */
        let y3 = y0 - 1.0f + 3.0f * G3;
        let z3 = z0 - 1.0f + 3.0f * G3;

        // Wrap the integer indices at 256, to avoid indexing perm.[] out of bounds */
        let ii = i &&& 0xff
        let jj = j &&& 0xff
        let kk = k &&& 0xff

        // Calculate the contribution from the four corners */
        let t0c = 0.6f - x0 * x0 - y0 * y0 - z0 * z0;
        let struct(t0, n0, t20, t40, gx0, gy0, gz0) =
            if t0c < 0.0f then struct(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f) // no influence
            else
                let gx0, gy0, gz0 = grad3(perm.[ii + perm.[jj + perm.[kk]]]);
                let t20 = t0c * t0c;
                let t40 = t20 * t20;
                let n0 = t40 * (gx0 * x0 + gy0 * y0 + gz0 * z0)
                struct(t0c, n0, t20, t40, gx0, gy0, gz0);

        let t1c = 0.6f - x1 * x1 - y1 * y1 - z1 * z1;
        let struct(t1, n1, t21, t41, gx1, gy1, gz1) =
            if t0c < 0.0f then struct(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f) // no influence
            else
                let gx1, gy1, gz1 = grad3(perm.[ii + i1 + perm.[jj + j1 + perm.[kk + k1]]]);
                let t21 = t1c * t1c;
                let t41 = t21 * t21;
                let n1 = t41 * (gx1 * x1 + gy1 * y1 + gz1 * z1);
                struct(t1c, n1, t21, t41, gx1, gy1, gz1);

        let t2c = 0.6f - x2 * x2 - y2 * y2 - z2 * z2;
        let struct(t2, n2, t22, t42, gx2, gy2, gz2) =
            if t0c < 0.0f then struct(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f) // no influence
            else
                let gx2, gy2, gz2 = grad3(perm.[ii + i2 + perm.[jj + j2 + perm.[kk + k2]]]);
                let t22 = t2c * t2c;
                let t42 = t22 * t22;
                let n2 = t42 * (gx2 * x2 + gy2 * y2 + gz2 * z2);
                struct(t2c, n2, t22, t42, gx2, gy2, gz2);

        let t3c = 0.6f - x3 * x3 - y3 * y3 - z3 * z3;
        let struct(t3, n3, t23, t43, gx3, gy3, gz3) =
            if t0c < 0.0f then struct(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f) // no influence
            else
                let gx3, gy3, gz3 = grad3(perm.[ii + 1 + perm.[jj + 1 + perm.[kk + 1]]]);
                let t23 = t3c * t3c;
                let t43 = t23 * t23;
                let n3 = t43 * (gx3 * x3 + gy3 * y3 + gz3 * z3);
                struct(t3c, n3, t23, t43, gx3, gy3, gz3);

        // Add contributions from each corner to get the final noise value.
        // The result is scaled to return values in the range [-1,1] */
        let value = 28.0f * (n0 + n1 + n2 + n3);

        //if( ( NULL != dnoise_dx ) && ( NULL != dnoise_dy ) && ( NULL != dnoise_dz ))
        if calcGrad then
            ////  A straight, unoptimised calculation would be like:
            ///     *dnoise_dx = -8.0f * t20 * t0 * x0 * dot(gx0, gy0, gz0, x0, y0, z0) + t40 * gx0;
            ///    *dnoise_dy = -8.0f * t20 * t0 * y0 * dot(gx0, gy0, gz0, x0, y0, z0) + t40 * gy0;
            ///    *dnoise_dz = -8.0f * t20 * t0 * z0 * dot(gx0, gy0, gz0, x0, y0, z0) + t40 * gz0;
            ///    *dnoise_dx += -8.0f * t21 * t1 * x1 * dot(gx1, gy1, gz1, x1, y1, z1) + t41 * gx1;
            ///    *dnoise_dy += -8.0f * t21 * t1 * y1 * dot(gx1, gy1, gz1, x1, y1, z1) + t41 * gy1;
            ///    *dnoise_dz += -8.0f * t21 * t1 * z1 * dot(gx1, gy1, gz1, x1, y1, z1) + t41 * gz1;
            ///    *dnoise_dx += -8.0f * t22 * t2 * x2 * dot(gx2, gy2, gz2, x2, y2, z2) + t42 * gx2;
            ///    *dnoise_dy += -8.0f * t22 * t2 * y2 * dot(gx2, gy2, gz2, x2, y2, z2) + t42 * gy2;
            ///    *dnoise_dz += -8.0f * t22 * t2 * z2 * dot(gx2, gy2, gz2, x2, y2, z2) + t42 * gz2;
            ///    *dnoise_dx += -8.0f * t23 * t3 * x3 * dot(gx3, gy3, gz3, x3, y3, z3) + t43 * gx3;
            ///    *dnoise_dy += -8.0f * t23 * t3 * y3 * dot(gx3, gy3, gz3, x3, y3, z3) + t43 * gy3;
            ///    *dnoise_dz += -8.0f * t23 * t3 * z3 * dot(gx3, gy3, gz3, x3, y3, z3) + t43 * gz3;
            ////
            let temp0 = t20 * t0 * (gx0 * x0 + gy0 * y0 + gz0 * z0);
            let dnoise_dx = temp0 * x0;
            let dnoise_dy = temp0 * y0;
            let dnoise_dz = temp0 * z0;
            let temp1 = t21 * t1 * (gx1 * x1 + gy1 * y1 + gz1 * z1);
            let dnoise_dx1 = dnoise_dx + temp1 * x1;
            let dnoise_dy1 = dnoise_dy + temp1 * y1;
            let dnoise_dz1 = dnoise_dz + temp1 * z1;
            let temp2 = t22 * t2 * (gx2 * x2 + gy2 * y2 + gz2 * z2);
            let dnoise_dx2 = dnoise_dx1 + temp2 * x2;
            let dnoise_dy2 = dnoise_dy1 + temp2 * y2;
            let dnoise_dz2 = dnoise_dz1 + temp2 * z2;
            let temp3 = t23 * t3 * (gx3 * x3 + gy3 * y3 + gz3 * z3);
            let dnoise_dx3 = (dnoise_dx2 + temp3 * x3) * -8.0f + t40 * gx0 + t41 * gx1 + t42 * gx2 + t43 * gx3;
            let dnoise_dy3 = (dnoise_dy2 + temp3 * y3) * -8.0f + t40 * gy0 + t41 * gy1 + t42 * gy2 + t43 * gy3;
            let dnoise_dz3 = (dnoise_dz2 + temp3 * z3) * -8.0f + t40 * gz0 + t41 * gz1 + t42 * gz2 + t43 * gz3;
            struct(value, dnoise_dx3 * 28.0f, dnoise_dy3 * 28.0f, dnoise_dz3 * 28.0f)
        else
            struct(value, 0.0f, 0.0f, 0.0f)

    // The skewing and unskewing factors are hairy again for the 4D case
    let private F4 = 0.309016994f; // F4 = (Math.sqrt(5.0)-1.0)/4.0
    let private G4 = 0.138196601f; // G4 = (5.0-Math.sqrt(5.0))/20.0
    //let Scaling4 = 27.0f

    //* 4D simplex noise with derivatives.
    //* If the last four arguments are not null, the analytic derivative
    //* (the 4D gradient of the scalar noise field) is also calculated.
    //*/
    let sample4 calcGrad (x: float32) (y: float32) (z: float32) (w: float32) =
        // Skew the (x,y,z,w) space to determine which cell of 24 simplices we're in
        let s = (x + y + z + w) * F4; // Factor for 4D skewing
        let xs = x + s;
        let ys = y + s;
        let zs = z + s;
        let ws = w + s;
        let i = floor(xs);
        let j = floor(ys);
        let k = floor(zs);
        let l = floor(ws);

        let t = float32(i + j + k + l) * G4; // Factor for 4D unskewing
        let X0 = float32(i) - t; // Unskew the cell origin back to (x,y,z,w) space
        let Y0 = float32(j) - t;
        let Z0 = float32(k) - t;
        let W0 = float32(l) - t;

        let x0 = x - X0;  // The x,y,z,w distances from the cell origin
        let y0 = y - Y0;
        let z0 = z - Z0;
        let w0 = w - W0;

        // For the 4D case, the simplex is a 4D shape I won't even try to describe.
        // To find out which of the 24 possible simplices we're in, we need to
        // determine the magnitude ordering of x0, y0, z0 and w0.
        // The method below is a reasonable way of finding the ordering of x,y,z,w
        // and then find the correct traversal order for the simplex we’re in.
        // First, six pair-wise comparisons are performed between each possible pair
        // of the four coordinates, and then the results are used to add up binary
        // bits for an integer index into a precomputed lookup table, simplex[].
        let c1 = if (x0 > y0) then 32 else 0;
        let c2 = if (x0 > z0) then 16 else 0;
        let c3 = if (y0 > z0) then 8 else 0;
        let c4 = if (x0 > w0) then 4 else 0;
        let c5 = if (y0 > w0) then 2 else 0;
        let c6 = if (z0 > w0) then 1 else 0;
        let c = c1 ||| c2 ||| c3 ||| c4 ||| c5 ||| c6; // '|' is mostly faster than '+'

        // simplex[c] is a 4-vector with the numbers 0, 1, 2 and 3 in some order.
        // Many values of c will never occur, since e.g. x>y>z>w makes x<z, y<w and x<w
        // impossible. Only the 24 indices which have non-zero entries make any sense.
        // We use a thresholding to set the coordinates in turn from the largest magnitude.
        // The number 3 in the "simplex" array is at the position of the largest coordinate.
        let i1 = if simplex.[c, 0] >= 3 then 1 else 0;
        let j1 = if simplex.[c, 1] >= 3 then 1 else 0;
        let k1 = if simplex.[c, 2] >= 3 then 1 else 0;
        let l1 = if simplex.[c, 3] >= 3 then 1 else 0;
        // The number 2 in the "simplex" array is at the second largest coordinate.
        let i2 = if simplex.[c, 0] >= 2 then 1 else 0;
        let j2 = if simplex.[c, 1] >= 2 then 1 else 0;
        let k2 = if simplex.[c, 2] >= 2 then 1 else 0;
        let l2 = if simplex.[c, 3] >= 2 then 1 else 0;
        // The number 1 in the "simplex" array is at the second smallest coordinate.
        let i3 = if simplex.[c, 0] >= 1 then 1 else 0;
        let j3 = if simplex.[c, 1] >= 1 then 1 else 0;
        let k3 = if simplex.[c, 2] >= 1 then 1 else 0;
        let l3 = if simplex.[c, 3] >= 1 then 1 else 0;
        // The fifth corner has all coordinate offsets = 1, so no need to look that up.

        let x1 = x0 - float32(i1) + G4; // Offsets for second corner in (x,y,z,w) coords
        let y1 = y0 - float32(j1) + G4;
        let z1 = z0 - float32(k1) + G4;
        let w1 = w0 - float32(l1) + G4;
        let x2 = x0 - float32(i2) + 2.0f * G4; // Offsets for third corner in (x,y,z,w) coords
        let y2 = y0 - float32(j2) + 2.0f * G4;
        let z2 = z0 - float32(k2) + 2.0f * G4;
        let w2 = w0 - float32(l2) + 2.0f * G4;
        let x3 = x0 - float32(i3) + 3.0f * G4; // Offsets for fourth corner in (x,y,z,w) coords
        let y3 = y0 - float32(j3) + 3.0f * G4;
        let z3 = z0 - float32(k3) + 3.0f * G4;
        let w3 = w0 - float32(l3) + 3.0f * G4;
        let x4 = x0 - 1.0f + 4.0f * G4; // Offsets for last corner in (x,y,z,w) coords
        let y4 = y0 - 1.0f + 4.0f * G4;
        let z4 = z0 - 1.0f + 4.0f * G4;
        let w4 = w0 - 1.0f + 4.0f * G4;

        // Wrap the integer indices at 256, to avoid indexing perm.[] out of bounds
        let ii = i &&& 0xff;
        let jj = j &&& 0xff;
        let kk = k &&& 0xff;
        let ll = l &&& 0xff;

        // Calculate the contribution from the five corners
        let t0c = 0.6f - x0 * x0 - y0 * y0 - z0 * z0 - w0 * w0;
        let struct(n0, t0, t20, t40, gx0, gy0, gz0, gw0) =
            if (t0c < 0.0f) then 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f // no influence
            else
                let t20 = t0c * t0c;
                let t40 = t20 * t20;
                let gx0, gy0, gz0, gw0 = grad4(perm.[ii + perm.[jj + perm.[kk + perm.[ll]]]]);
                let n0 = t40 * (gx0 * x0 + gy0 * y0 + gz0 * z0 + gw0 * w0);
                struct(n0, t0c, t20, t40, gx0, gy0, gz0, gw0)

        let t1c = 0.6f - x1 * x1 - y1 * y1 - z1 * z1 - w1 * w1;
        let struct(n1, t1, t21, t41, gx1, gy1, gz1, gw1) =
            if (t1c < 0.0f) then 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f // no influence
            else
                let t21 = t1c * t1c;
                let t41 = t21 * t21;
                let gx1, gy1, gz1, gw1 = grad4(perm.[ii + i1 + perm.[jj + j1 + perm.[kk + k1 + perm.[ll + l1]]]]);
                let n1 = t41 * (gx1 * x1 + gy1 * y1 + gz1 * z1 + gw1 * w1);
                struct(n1, t1c, t21, t41, gx1, gy1, gz1, gw1)

        let t2c = 0.6f - x2 * x2 - y2 * y2 - z2 * z2 - w2 * w2;
        let struct(n2, t2, t22, t42, gx2, gy2, gz2, gw2) =
            if (t2c < 0.0f) then 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f // no influence
            else
                let t22 = t2c * t2c;
                let t42 = t22 * t22;
                let gx2, gy2, gz2, gw2 = grad4(perm.[ii + i2 + perm.[jj + j2 + perm.[kk + k2 + perm.[ll + l2]]]]);
                let n2 = t42 * (gx2 * x2 + gy2 * y2 + gz2 * z2 + gw2 * w2);
                struct(n2, t2c, t22, t42, gx2, gy2, gz2, gw2)

        let t3c = 0.6f - x3 * x3 - y3 * y3 - z3 * z3 - w3 * w3;
        let struct(n3, t3, t23, t43, gx3, gy3, gz3, gw3) =
            if (t3c < 0.0f) then 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f // no influence
            else
                let t23 = t3c * t3c;
                let t43 = t23 * t23;
                let gx3, gy3, gz3, gw3 = grad4(perm.[ii + i3 + perm.[jj + j3 + perm.[kk + k3 + perm.[ll + l3]]]]);
                let n3 = t43 * (gx3 * x3 + gy3 * y3 + gz3 * z3 + gw3 * w3);
                struct(n3, t3c, t23, t43, gx3, gy3, gz3, gw3)

        let t4c = 0.6f - x4 * x4 - y4 * y4 - z4 * z4 - w4 * w4;
        let struct(n4, t4, t24, t44, gx4, gy4, gz4, gw4) =
            if (t4c < 0.0f) then 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f // no influence
            else
                let t24 = t4c * t4c;
                let t44 = t24 * t24;
                let gx4, gy4, gz4, gw4 = grad4(perm.[ii + 1 + perm.[jj + 1 + perm.[kk + 1 + perm.[ll + 1]]]]);
                let n4 = t44 * (gx4 * x4 + gy4 * y4 + gz4 * z4 + gw4 * w4);
                struct(n4, t4c, t24, t44, gx4, gy4, gz4, gw4)

        // Sum up and scale the result to cover the range [-1,1]
        let value = 27.0f * (n0 + n1 + n2 + n3 + n4); // The scale factor is preliminary!

        // Compute derivative, if requested by supplying non-null pointers
        // for the last four arguments */
        //if( ( NULL != dnoise_dx ) && ( NULL != dnoise_dy ) && ( NULL != dnoise_dz ) && ( NULL != dnoise_dw ) )
        if calcGrad then
            //  A straight, unoptimised calculation would be like:
            //*     *dnoise_dx = -8.0f * t20 * t0 * x0 * dot(gx0, gy0, gz0, gw0, x0, y0, z0, w0) + t40 * gx0;
            //*    *dnoise_dy = -8.0f * t20 * t0 * y0 * dot(gx0, gy0, gz0, gw0, x0, y0, z0, w0) + t40 * gy0;
            //*    *dnoise_dz = -8.0f * t20 * t0 * z0 * dot(gx0, gy0, gz0, gw0, x0, y0, z0, w0) + t40 * gz0;
            //*    *dnoise_dw = -8.0f * t20 * t0 * w0 * dot(gx0, gy0, gz0, gw0, x0, y0, z0, w0) + t40 * gw0;
            //*    *dnoise_dx += -8.0f * t21 * t1 * x1 * dot(gx1, gy1, gz1, gw1, x1, y1, z1, w1) + t41 * gx1;
            //*    *dnoise_dy += -8.0f * t21 * t1 * y1 * dot(gx1, gy1, gz1, gw1, x1, y1, z1, w1) + t41 * gy1;
            //*    *dnoise_dz += -8.0f * t21 * t1 * z1 * dot(gx1, gy1, gz1, gw1, x1, y1, z1, w1) + t41 * gz1;
            //*    *dnoise_dw = -8.0f * t21 * t1 * w1 * dot(gx1, gy1, gz1, gw1, x1, y1, z1, w1) + t41 * gw1;
            //*    *dnoise_dx += -8.0f * t22 * t2 * x2 * dot(gx2, gy2, gz2, gw2, x2, y2, z2, w2) + t42 * gx2;
            //*    *dnoise_dy += -8.0f * t22 * t2 * y2 * dot(gx2, gy2, gz2, gw2, x2, y2, z2, w2) + t42 * gy2;
            //*    *dnoise_dz += -8.0f * t22 * t2 * z2 * dot(gx2, gy2, gz2, gw2, x2, y2, z2, w2) + t42 * gz2;
            //*    *dnoise_dw += -8.0f * t22 * t2 * w2 * dot(gx2, gy2, gz2, gw2, x2, y2, z2, w2) + t42 * gw2;
            //*    *dnoise_dx += -8.0f * t23 * t3 * x3 * dot(gx3, gy3, gz3, gw3, x3, y3, z3, w3) + t43 * gx3;
            //*    *dnoise_dy += -8.0f * t23 * t3 * y3 * dot(gx3, gy3, gz3, gw3, x3, y3, z3, w3) + t43 * gy3;
            //*    *dnoise_dz += -8.0f * t23 * t3 * z3 * dot(gx3, gy3, gz3, gw3, x3, y3, z3, w3) + t43 * gz3;
            //*    *dnoise_dw += -8.0f * t23 * t3 * w3 * dot(gx3, gy3, gz3, gw3, x3, y3, z3, w3) + t43 * gw3;
            //*    *dnoise_dx += -8.0f * t24 * t4 * x4 * dot(gx4, gy4, gz4, gw4, x4, y4, z4, w4) + t44 * gx4;
            //*    *dnoise_dy += -8.0f * t24 * t4 * y4 * dot(gx4, gy4, gz4, gw4, x4, y4, z4, w4) + t44 * gy4;
            //*    *dnoise_dz += -8.0f * t24 * t4 * z4 * dot(gx4, gy4, gz4, gw4, x4, y4, z4, w4) + t44 * gz4;
            //*    *dnoise_dw += -8.0f * t24 * t4 * w4 * dot(gx4, gy4, gz4, gw4, x4, y4, z4, w4) + t44 * gw4;
            //*/
            let temp0 = t20 * t0 * (gx0 * x0 + gy0 * y0 + gz0 * z0 + gw0 * w0);
            let dnoise_dx = temp0 * x0;
            let dnoise_dy = temp0 * y0;
            let dnoise_dz = temp0 * z0;
            let dnoise_dw = temp0 * w0;
            let temp1 = t21 * t1 * (gx1 * x1 + gy1 * y1 + gz1 * z1 + gw1 * w1);
            let dnoise_dx1 = dnoise_dx + temp1 * x1;
            let dnoise_dy1 = dnoise_dy + temp1 * y1;
            let dnoise_dz1 = dnoise_dz + temp1 * z1;
            let dnoise_dw1 = dnoise_dw + temp1 * w1;
            let temp2 = t22 * t2 * (gx2 * x2 + gy2 * y2 + gz2 * z2 + gw2 * w2);
            let dnoise_dx2 = dnoise_dx1 + temp2 * x2;
            let dnoise_dy2 = dnoise_dy1 + temp2 * y2;
            let dnoise_dz2 = dnoise_dz1 + temp2 * z2;
            let dnoise_dw2 = dnoise_dw1 + temp2 * w2;
            let temp3 = t23 * t3 * (gx3 * x3 + gy3 * y3 + gz3 * z3 + gw3 * w3);
            let dnoise_dx3 = dnoise_dx2 + temp3 * x3;
            let dnoise_dy3 = dnoise_dy2 + temp3 * y3;
            let dnoise_dz3 = dnoise_dz2 + temp3 * z3;
            let dnoise_dw3 = dnoise_dw2 + temp3 * w3;
            let temp4 = t24 * t4 * (gx4 * x4 + gy4 * y4 + gz4 * z4 + gw4 * w4);
            let dnoise_dx4 = dnoise_dx3 + temp4 * x4;
            let dnoise_dy4 = dnoise_dy3 + temp4 * y4;
            let dnoise_dz4 = dnoise_dz3 + temp4 * z4;
            let dnoise_dw4 = dnoise_dw3 + temp4 * w4;
            let dnoise_dx5 = dnoise_dx4 * -8.0f + t40 * gx0 + t41 * gx1 + t42 * gx2 + t43 * gx3 + t44 * gx4;
            let dnoise_dy5 = dnoise_dy4 * -8.0f + t40 * gy0 + t41 * gy1 + t42 * gy2 + t43 * gy3 + t44 * gy4;
            let dnoise_dz5 = dnoise_dz4 * -8.0f + t40 * gz0 + t41 * gz1 + t42 * gz2 + t43 * gz3 + t44 * gz4;
            let dnoise_dw5 = dnoise_dw4 * -8.0f + t40 * gw0 + t41 * gw1 + t42 * gw2 + t43 * gw3 + t44 * gw4;
            struct(value, dnoise_dx5 * 28.0f, dnoise_dy5 * 28.0f, dnoise_dz5 * 28.0f, dnoise_dw5 * 28.0f)
        else
            struct(value, 0.0f, 0.0f, 0.0f, 0.0f)

type Noise() =
    static member Sample(x) =
        let struct(s, _) = GradientNoise.sample1 false x
        s
        
    static member Sample(p : Vector2) =
        SimplexNoise.sample2 p.X p.Y

    static member Sample(p : Vector3) =
        SimplexNoise.sample3 p.X p.Y p.Z

    static member Sample(p : Vector4) =
        SimplexNoise.sample4 p.X p.Y p.Z p.W
                