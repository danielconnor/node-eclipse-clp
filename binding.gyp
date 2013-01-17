{
  'targets': [
    {
      'target_name': 'libeclipse',

      'configurations': {
        'Release': {
          'msvs_settings': {
            'VCCLCompilerTool': {
              'ExceptionHandling': 1, #Sync
              'DisableSpecificWarnings': [
                '4506'
              ]
            },
          },
        },
      },

      'include_dirs': [
        'eclipse/include'
      ],

      'link_settings': {
        'libraries': [
          '../eclipse/lib/eclipse.lib',
        ],
      },

      'sources': [
        'src/libeclipse.cc',
        'src/atom.cc',
        'src/functor.cc',
        'src/ref.cc',
        'src/compound.cc',
        'src/util.cc'
      ],
    }
  ]
}