from docutils import nodes, utils

'''
This module defines a Sphinx plugin which enables the use of a
':ref_guide:' role within Sphinx documentation. The link targets
are then generated using the 'ref_guide_base' (configuration
variable) as a base. This is set in the 'conf.py' of the documentation
and enables link targets to be configured at build time.

'''


def setup(app):
    '''
    Sets-up this Sphinx plugin.

    :param app: the Sphinx application object.
    :type app: :py:class:`sphinx.application.Sphinx`

    '''
    def ref_link_role(role, rawtext, text, lineno, inliner, options={},
                      content=[]):
        '''
        Handler routine called when the ':ref_guide:' role is encountered.

        Modifies the supplied link text by pre-pending the text stored in
        the 'ref_guide_base' Sphinx configuration variable.

        '''
        items = text.split()
        ref = app.config.ref_guide_base + items[-1]
        if len(items) > 1:
            # The link had a name associated with it so reconstruct it.
            name = ' '.join(items[:-1])
        else:
            name = utils.unescape(ref)
        # Construct the new reference.
        node = nodes.reference(rawtext, name, refuri=ref, **options)
        return [node], []

    # Define a new Sphinx configuration variable that will be used to
    # store the base URL to use when generating links.
    app.add_config_value('ref_guide_base', 'http://localhost/', False)

    # Define the new role and assign it our new handler.
    app.add_role('ref_guide', ref_link_role)
