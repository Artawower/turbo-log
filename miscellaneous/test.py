class TestClass:
    def test(self):
        hello_world = "Hello world"
        another_var = 12
        result = f"{hello_world} {another_var}"
        return hello_world

    def test2(
        self,
        a,
        b,
    ):
        hello_world = "Hello world"
        another_var = 12
        b = [1, 2, 3, 4, 5, 6, 7, 8, 9]
        result = f"{hello_world} {another_var}"

        # print("[line 7][test.py] TCL:  hello_world: ", hello_world)
        return hello_world
