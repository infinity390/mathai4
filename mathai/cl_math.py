import numpy as np


class CLMath:
    type_arr = np.ndarray
    # ---------- Creation ----------
    @staticmethod
    def to_gpu(x):
        if isinstance(x, np.ndarray):
            return x
        return np.asarray(x, dtype=np.float32)
    
    @staticmethod
    def zeros(*shape):
        return np.zeros(shape, dtype=np.float32)

    @staticmethod
    def randos(low, high, *shape):
        return np.random.uniform(low, high, shape).astype(np.float32)
    @staticmethod
    def wrap(data):
        return np.expand_dims(data, axis=0)
    # ---------- Shape ----------

    @staticmethod
    def flatten(A):
        return A.reshape(A.shape[0], -1)

    @staticmethod
    def reshape(data, *shape):
        data = CLMath.to_gpu(data)

        # reshape(A, (2,3))
        if len(shape) == 1 and isinstance(shape[0], (tuple, list)):
            shape = tuple(shape[0])

        # reshape(A, 2, 3)
        else:
            shape = tuple(shape)

        return np.reshape(data, shape, order="F")

    @staticmethod
    def transpose(A):
        return np.transpose(A)

    @staticmethod
    def shape(A):
        return list(A.shape)

    @staticmethod
    def vec(A):
        return A.reshape((-1, 1), order="F")

    @staticmethod
    def diag(v):
        return np.diag(np.ravel(v))

    @staticmethod
    def broadcast(A, rows):
        if A.shape[0] == rows:
            return A
        if A.shape[0] != 1:
            raise ValueError
        return np.broadcast_to(A, (rows,) + A.shape[1:]).copy()

    # ---------- Elementwise ----------

    @staticmethod
    def hadamard(*args):
        out = args[0]
        for x in args[1:]:
            out = out * x
        return out

    @staticmethod
    def matadd(*args):
        out = args[0]
        for x in args[1:]:
            out = out + x
        return out

    @staticmethod
    def exp(x):
        return np.exp(x)

    @staticmethod
    def tanh(x):
        return np.tanh(x)

    @staticmethod
    def sigmoid(x):
        return 1.0 / (1.0 + np.exp(-x))

    @staticmethod
    def relu(x):
        return np.maximum(x, 0)

    @staticmethod
    def drelu(x):
        return (x > 0).astype(np.float32)

    @staticmethod
    def pow(x, p):
        return np.power(x, p)

    # ---------- Linear Algebra ----------

    @staticmethod
    def matmul(*args):
        def rank(x):
            return np.asarray(x).ndim

        out = np.asarray(args[0])

        for x in args[1:]:
            x = np.asarray(x)

            r1 = rank(out)
            r2 = rank(x)

            # scalar cases
            if r1 == 0 or r2 == 0:
                out = out * x

            # matrix @ matrix
            elif r1 == 2 and r2 == 2:
                out = np.matmul(out, x)

            # batch matrix @ matrix
            elif r1 == 3 and r2 == 2:
                out = np.matmul(out, x)

            # matrix @ batch matrix
            elif r1 == 2 and r2 == 3:
                out = np.matmul(out[np.newaxis, ...], x)

            # batch matrix @ batch matrix
            elif r1 == 3 and r2 == 3:
                if out.shape[0] == 1:
                    out = np.matmul(out, x)
                elif x.shape[0] == 1:
                    out = np.matmul(out, x)
                else:
                    assert out.shape[0] == x.shape[0]
                    out = np.matmul(out, x)

            else:
                raise ValueError(
                    f"Unsupported matmul ranks ({r1}, {r2})"
                )

        return out
    @staticmethod
    def kronecker(*args):
        out = args[0]
        for x in args[1:]:
            out = np.kron(out, x)
        return out

    @staticmethod
    def identity(n):
        return np.eye(n, dtype=np.float32)

    @staticmethod
    def len(A):
        return A.shape[0]

    @staticmethod
    def index(A, n):
        return A[n]

    # ---------- CNN ----------
    @staticmethod
    def conv(X, K):
        X = np.asarray(X)
        K = np.asarray(K)

        B, H, W, C = X.shape
        _, Kh, Kw, _ = K.shape

        OH = H - Kh + 1
        OW = W - Kw + 1

        out = np.empty((B, OH, OW, 1), dtype=X.dtype)

        for i in range(OH):
            for j in range(OW):
                patch = X[:, i:i+Kh, j:j+Kw, :]
                out[:, i, j, 0] = np.sum(patch * K[0], axis=(1,2,3))

        return out

    @staticmethod
    def im2col(image, kernel):
        image = np.asarray(image)
        kernel = np.asarray(kernel)

        B, H, W, C = image.shape
        _, Kh, Kw, _ = kernel.shape

        OH = H - Kh + 1
        OW = W - Kw + 1

        P = Kh * Kw * C

        cols = np.zeros((B * OH * OW, B * P), dtype=image.dtype)

        row = 0

        for b in range(B):
            for i in range(OH):
                for j in range(OW):
                    cols[row, b*P:(b+1)*P] = image[b, i:i+Kh, j:j+Kw, :].reshape(-1)
                    row += 1

        return cols

    @staticmethod
    def col2im(kernel, image):
        kernel = np.asarray(kernel)
        image = np.asarray(image)

        B, H, W, C = image.shape
        Kb, Kh, Kw, _ = kernel.shape

        assert Kb == 1 or Kb == B

        OH = H - Kh + 1
        OW = W - Kw + 1

        rows = B * OH * OW
        cols = B * H * W * C

        J = np.zeros((rows, cols), dtype=kernel.dtype)

        row = 0

        for b in range(B):
            kb = 0 if Kb == 1 else b

            for i in range(OH):
                for j in range(OW):

                    for u in range(Kh):
                        for v in range(Kw):
                            for ch in range(C):

                                col = (
                                    (((b * H + (i + u)) * W + (j + v)) * C)
                                    + ch
                                )

                                J[row, col] = kernel[kb, u, v, ch]

                    row += 1

        return J

    @staticmethod
    def commutation(m, n):
        size = m * n
        K = np.zeros((size, size), dtype=np.float32)

        for i in range(m):
            for j in range(n):
                src = j * m + i
                dst = i * n + j
                K[dst, src] = 1

        return K
