class TestClass {
  private t: any;
  private b: string;
  private myAnotherVariable: any;

  constructor(t: any, b: string, myAnotherVariable: number) {
    this.t = t;
    this.b = b;
    this.myAnowtherVariable = myAnotherVariable;
    console.log(
      "This message will not be comment, cause it made by programmers."
    );
  }
}

function myFuncWithEmptyBody(qwwe) {}

function test(): string {
  const hello = "Hello";
  return hello;
}

function testFuncWithBigSignature(
  name: string,
  age: number,
  patronymic?: string
): string {
  return `${name} is ${age} years old. ${patronymic}`;
}

function test() {
  const foo = 1;
  const bar = 2;
  const b = [1, 2, 3, 10, 12, 22, 33, 44, 15];

  const a = 4;
  if ((a = 1)) {
  }

  for (let b = 0; b < 4; b++) {}
}

const a = () => ({});

function test() {
  const foo = 1;

  const bar = 2;
}

const a = (k) => {
  const b = k;
};
