
__all__ = ['Component']

import re
from joiner import Joiner

DEBUG = True
parts_re = re.compile(r'%\([\w.]+\)[rs]')
killline_re = re.compile(r'.*[<]KILLLINE[>].*(\n|$)')

class Branch(object):
    """ Represents a branch of a Component tree.
    """
    def __init__(self, *items):
        self.items = items

    def __add__(self, other):
        if isinstance(other, Component):
            return type(self)(*(self.items + (other,)))
        return NotImplemented

    def get_attr(self, name, default=NotImplemented):
        """ Search a branch component that contains attribute with
        given name and return attribute value. If the name is in
        a form ``clsname.attrname`` then the matching component must
        have class name equal to ``clsname``. If attribute is not
        found, raise an attribute error.
        """
        if '.' in name:
            classname, attrname = name.split('.')
        else:
            classname, attrname = None, name
        for c in reversed(self.items):
            if classname and classname!=type(c).__name__:
                continue
            if hasattr(c, attrname):
                return getattr(c, attrname)
        if default is NotImplemented:
            n = '-'.join([type(c).__name__ for c in self.items])
            raise AttributeError("component branch %r does not have attribute with name %r" % (n, name))
        return default

    def get_left(self, component):
        if not self.items:
            return []
        parent = self.items[-1]
        r = []
        for leaf in parent.leafs:
            if leaf==component:
                return r
            r.append(leaf)
        return r

    def find_component_with_view(self, view_name, view_result):
        if not self.items:
            return
        return self.items[0].find_component_with_view(view_name, view_result)
        
class Component(object):
    """ Base class for Component subclasses.

    This class implements a certain combination of Composite
    and Template method patterns.

    A component contains data that are available as attributes.  One
    of the attributes is ``leafs`` that is a list of subcomponents.
    Other attributes are defined by the definition of Component
    subclass.

    A parent component can make data requests to its leafs. The data
    will be used to fill out the component templates. The request
    is done by submitting the following pair to a leafs:

    #. the name of a view template part for which data is requested,
       (leaves must know how to serve their parents).

    #. a parent branch that provides parent data as well as the
       possibility to access the data of neighboring leaves.

    When the returned values from the component leafs are collected,
    then the templates will be realized.

    Implementation notes:

    #. data requests are done with ``request_data(subs_name,
       parents)`` method where ``parents`` argument contains a parent branch.

    #. a view template is a string containing ``%(...)s`` (so-called
       the parts of a view template), components can define different
       views in ``templates`` dictionary attribute.

    #. the results of component data requestes are collected
       in ``Joiner`` instances (see joiner.py).

    #. the view is obtained by calling ``get_view(view_name, parents=Branch())``::

         <view template> % dict(<name of view template part> = <Joiner instance>)

    #. realization of a template is returned by the ``realize()`` method.

    #. component classes can redefine the following methods:

         #. ``__init__`` --- to define component specific data
            attributes

         #. ``request_data`` --- to connect leafs with parents

         #. ``realize`` --- to return the final realization of a
            component, it can be anything, including executing
            external commands

    """

    # templates dictionary contains mappings:
    #   {<view name>:<template string>}
    templates = dict()

    # template_options dictionary contains mappings:
    #   {<name of template part>:<options dictionary to Joiner constructor>}
    template_options = dict()

    # set True if get_view can call request_data:
    request_own_data = False

    def __init__(self, *leafs):
        self.leafs = []
        self._nof_saved_leafs = [0]
        map(self.add, leafs)

    @classmethod
    def _check_options(cls, options, *expected_names):
        unknown_names = set(options).difference(expected_names)
        if unknown_names:
            raise ValueError('%s: unknown options %r' % (cls.__name__, unknown_names))

    def add(self, obj):
        if isinstance(obj, Component):
            if isinstance(obj, type(self)):
                self.leafs.extend(obj.leafs)
            else:
                self.leafs.append(obj)
        elif isinstance(obj, list):
            map(self.add, obj)
        else:
            raise TypeError("%s.add: %r" % (type(self).__name__, type(obj)))

    def request_data(self, subs_name, parent_branch):
        """ Called by parent component to request data that
        is used to fill out parent component named view. Return
        None if component does not add any data to a parent view.
        """
        return None

    @classmethod
    def _get_template(cls, name):
        if name in cls.templates:
            return cls.templates[name]
        if cls is Component:
            return None
        for c in cls.__bases__:
            if issubclass(c, Component):
                r = c._get_template(name)
                if r is not None:
                    return r
        return None

    @classmethod
    def _get_template_options(cls, name):
        if name in cls.template_options:
            return cls.template_options[name]
        if cls is Component:
            return None
        for c in cls.__bases__:
            if issubclass(c, Component):
                r = c._get_template_options(name)
                if r is not None:
                    return r
        return None

    @classmethod
    def _show_views(cls, d=None):
        flag = False
        if d is None:
            d = {}
            flag = True

        for name, template in cls.templates.items():
            if name not in d:
                d[name] = template
                    
        if cls is Component:
            pass
        else:
            for c in cls.__bases__:
                if issubclass(c, Component):
                    c._show_views(d)
        if flag:
            print 'Class %r instances provide the following views:' % (cls)
            for name, template in d.items():
                print '%s: %r' % (name, template)

    def get_view(self, view_name, parents=None, ignore_missing_view=False):
        """ Return a named view of a component using parents.
        """
        template = self._get_template(view_name)
        if template is None:
            if ignore_missing_view:
                return
            if DEBUG:
                print '%s does not provide view %r' % (type(self).__name__, view_name)
            return

        if parents is None:
            parents = Branch()
        elif isinstance(parents, Component):
            parents = Branch(parents)

        branch = parents + self

        part_names = []
        part_containers = {}
        for n1 in parts_re.findall(template):
            n = n1[2:-2]
            if '..' in n:
                name, subs_name = n.split('..')
            else:
                name = subs_name = n
            opts = self._get_template_options(name)
            if opts is not None:
                part_names.append((subs_name, n))
                v = Joiner(**opts)
            else:
                assert name==subs_name==n,`name, subs_name, n`
                try:
                    v = branch.get_attr(n)
                except AttributeError, msg:
                    if DEBUG:
                        print '%s, using value %r' % (msg, n1)
                    v = n1
            part_containers[n] = v

        if self.request_own_data:
            leafs = [self] + self.leafs
        else:
            leafs = self.leafs

        for leaf in leafs:        
            for subs_name, n in part_names:
                data = leaf.request_data(subs_name, branch)
                if data is not None:
                    part_containers[n] += data
                else:
                    if DEBUG and 0:
                        print '%s.get_view:%s does not provide %r' % (type(self).__name__, type(leaf).__name__, name)

        result = template % part_containers

        result = killline_re.sub('', result)

        return result

    def realize(self):
        """ Return final realization of a component.

        When implementing realize method, use the following template::

          self.save()    # save current internal state
          ...
          result = ...
          self.restore() # restore initial internal state
          return result

        Note that realize methods may change the original states of
        component trees, therefore it is recommended to use save and
        restore methods when entering and leaving the realize method.
        """
        self.save()
        result = self.get_view('string')
        self.restore()
        return result

    def save(self):
        """ Save current internal state.
        """
        self._nof_saved_leafs.append(len(self.leafs))
        [c.save() for c in self.leafs]

    def restore(self):
        """ Restore previous internal state.
        """
        if not self._nof_saved_leafs:
            n = 0
        else:
            n = self._nof_saved_leafs.pop()
        while len(self.leafs) > n:
            self.leafs.pop()
        [c.restore() for c in self.leafs]

    def find_component_with_view(self, view_name, view_result):
        old = self.request_own_data
        self.request_own_data = False
        v = self.get_view(view_name, ignore_missing_view=True)
        if v==view_result:
            r = self
        else:
            r = None
            for leaf in self.leafs:
                r = leaf.find_component_with_view(view_name, view_result)
                if r is not None:
                    break
        self.request_own_data = old
        return r


########### Test code follows ############

class ExampleModule(Component):

    templates = dict(
        source = '''\
# module: %(name)s
%(functions)s
'''
        )

    template_options = dict(
        functions = dict()
        )

    def __init__(self, name, *leafs):
        Component.__init__(self, *leafs)
        self.name = name

class ExampleFunction(Component):

    templates = dict(
        source = '''\
# module name: %(ExampleModule.name)s
def %(name)s(%(arguments)s):
    %(code)s
'''
        )

    template_options = dict(
        arguments = dict(separator=', '),
        code = dict(default='pass')
    )

    def __init__(self, name, *leafs):
        Component.__init__(self, *leafs)
        self.name = name

    def request_data(self, subs_name, parents):
        if subs_name=='functions':
            return self.get_view('source', parents)

    def realize(self):
        return self.get_view('source')

class ExampleArgument(Component):

    templates = dict()

    def __init__(self, name, *leafs):
        Component.__init__(self, *leafs)
        self.name = name

    def request_data(self, subs_name, parents):
        if subs_name=='arguments':
            return self.name

class ExampleCode(Component):

    templates = dict()

def _test():
    a = ExampleArgument('a')
    b = ExampleArgument('b')
    f = ExampleFunction('foo', a, b)
    assert f.get_view('source')=='# module name: %(ExampleModule.name)s\ndef foo(a, b):\n    pass\n'
    m = ExampleModule('m', f)
    assert m.get_view('source')=='# module: m\n# module name: m\ndef foo(a, b):\n    pass\n\n'

if __name__ == '__main__':
    _test()
    print 'ok'
