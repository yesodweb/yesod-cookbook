This page (not yet complete) demonstrates different ways you may wish to override the `approot` method.

## Scaffolding

Get a static application root from runtime configuration stored in the foundation datatype:

    approot = ApprootMaster $ appRoot . appSettings