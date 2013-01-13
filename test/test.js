var
  eclipse = require('../eclipse');
  assert = require('assert');

describe("eclipse", function() {
  assert.equal(eclipse.init(), eclipse.status.EC_succeed);

  describe('Ref', function() {

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

});

