{
  "targets": [
    {
      "target_name": "libeclipse",

      'include_dirs': [
        'eclipse/include'
      ],

      'link_settings': {
        'libraries': [
          '../eclipse/lib/eclipse.lib',
        ],
      },

      "sources": [
        "src/libeclipse.cc",
        "src/atom.cc",
        "src/functor.cc",
        "src/ref.cc"
      ]
    }
  ]
}