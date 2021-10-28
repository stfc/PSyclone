from docutils import nodes, utils


def setup(app):
    def api_link_role(role, rawtext, text, lineno, inliner, options={},
                      content=[]):
        #import pdb; pdb.set_trace()
        items = text.split()
        ref = app.config.apilinks_base + items[-1]
        if len(items) == 2:
            name = items[0]
        else:
            name = utils.unescape(ref)

        node = nodes.reference(rawtext, name, refuri=ref, **options)
        return [node], []
    app.add_config_value('apilinks_base', 'http://localhost/', False)
    app.add_role('apilink', api_link_role)
