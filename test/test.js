var
  eclipse = require('../eclipse');
  assert = require('assert');

describe("eclipse", function() {
  assert.equal(eclipse.init(), eclipse.status.EC_succeed);


  describe('Ref', function() {


    describe("#value", function() {

      it("should return undefined when it has not been assigned a value", function() {

        assert.strictEqual(new eclipse.Ref().value, undefined);

      });

      it("should be able to be assigned a value", function() {
        var r = new eclipse.Ref(),
          val = 10;

        r.value = val;

        assert.strictEqual(val, r.value);

      });

      it("should throw an error if it is assigned a value of an unsupported type", function() {
        var r = new eclipse.Ref();

        assert.throws(function() {
          r.value = undefined;
        },
        TypeError);

      });

    });


  });

  describe('Atom', function() {
    describe('#name', function() {
      it('should return the same name as the atom was created with', function() {
        var name = "atom";
        assert.equal(name, new eclipse.Atom(name).name);
      });
    });
  });

  describe('Functor', function() {
    var name = "writeln",
      arity = 1,
      functor = new eclipse.Functor(name, arity);

    describe('#name', function() {
      it('should return the same name as the functor was created with', function() {
        assert.equal(name, functor.name);
      });
    });

    describe('#arity', function() {
      it('should return the same arity as the functor was created with', function() {
        assert.equal(arity, functor.arity);
      });
    });
  });

  describe("term", function() {
    var writeln = new eclipse.Functor("writeln", 1),
      args = ["1"],
      term = eclipse.term(writeln, args[0]);

    it("should return an instance of Compound", function() {

      assert(term instanceof eclipse.Compound);

    });


    it("should throw an error if an unsupported type is used as an argument", function() {
      assert.throws(function() {
        eclipse.term(writeln, undefined);
      },
      TypeError);
    });

    describe("#functor", function() {

      it("should be the same as the one it was created with", function() {

        assert.strictEqual(term.functor.name, writeln.name);
        assert.strictEqual(term.functor.arity, writeln.arity);

      });

    });

    describe("#[]", function() {

      it("the arguments should contain the same ones as it was created with", function() {

        args.forEach(function(v, i) {
          assert.strictEqual(term[i + 1], v);
        });

      });

      it("[0] should contain .functor", function() {

        assert.strictEqual(term.functor.name, term[0].name);
        assert.strictEqual(term.functor.arity, term[0].arity);

      });

    });


  });
});

